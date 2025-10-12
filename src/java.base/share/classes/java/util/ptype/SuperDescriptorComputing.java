package java.util.ptype;

import java.lang.reflect.*;
import java.util.ptype.TypeDescriptor.Properties.Property;

final class SuperDescriptorComputing {

    static ImmutableHashMap<Class<?>, ClassDescriptor> buildSuperMap(ClassDescriptor concrete) {
        Utils.requireNonNull(concrete);
        var set = buildRegularClassMap(concrete);
        return ImmutableHashMap.superTypeMap(set);
    }

    static ImmutableHashMap<Class<?>, ClassDescriptor> buildSuperMap(HiddenClassDescriptor concrete) {
        Utils.requireNonNull(concrete);
        return buildHiddenClassMap(concrete);
    }

    private static ImmutableHashMap<Class<?>, ClassDescriptor> buildHiddenClassMap(HiddenClassDescriptor concrete) {
        var finalSet = new HashSet<>(EXTRACTOR);
        concrete.forEachDirectSuperType(new Consumer<ClassDescriptor>() {
            @Override
            public void accept(ClassDescriptor concrete) {
                finalSet.addAll(buildRegularClassMap(concrete));
            }
        });
        return ImmutableHashMap.superTypeMap(finalSet);
    }

    private static HashSet<ClassDescriptor> buildRegularClassMap(ClassDescriptor concrete) {
        var current = concrete.type();
        var allMappings = new HashSet<>(EXTRACTOR);

        for (var genericInterface : current.getGenericInterfaces()) {
            var superDescriptor = (ClassDescriptor) mapType(concrete, genericInterface);
            var mappings = buildRegularClassMap(superDescriptor);
            allMappings.addAll(mappings);

            // if the type is not parameterized, we can discard it
            if (!superDescriptor.hasArgument()) {
                continue;
            }

            superDescriptor.superTypes = ImmutableHashMap.superTypeMap(mappings);
            allMappings.add(superDescriptor);
        }

        if (current.getGenericSuperclass() != null && shouldProcess(current.getSuperclass())) {
            var superDescriptor = (ClassDescriptor) mapType(concrete, current.getGenericSuperclass());
            var mappings = buildRegularClassMap(superDescriptor);
            allMappings.addAll(mappings);

            // only add if the type is parameterized
            if (superDescriptor.hasArgument()) {
                superDescriptor.superTypes = ImmutableHashMap.superTypeMap(mappings);
                allMappings.add(superDescriptor);
            }
        }

        return allMappings;
    }

    //region mapping

    private static TypeDescriptor[] mapTypes(ClassDescriptor concrete, Type[] typeArguments) {
        var dest = new TypeDescriptor[typeArguments.length];
        for (var i = 0; i < typeArguments.length; i++) {
            var result = mapType(concrete, typeArguments[i]);
            dest[i] = result;
        }
        return dest;
    }

    private static TypeDescriptor mapType(ClassDescriptor concrete, Type typeArgument) {
        switch (typeArgument) {
            case ParameterizedType parameterizedType:
                return mapParameterizedType(concrete, parameterizedType);
            case Class<?> clazz:
                return mapClass(clazz);
            case TypeVariable<?> typeVariable:
                return mapTypeVariable(concrete, typeVariable);
            case GenericArrayType array:
                return mapArrayType(concrete, array);
            case WildcardType _:
                return mapWildcard();
            default:
                var message = Utils.join("Unexpected type: ", typeArgument);
                throw new AssertionError(message);
        }
    }

    private static TypeDescriptor mapTypeVariable(ClassDescriptor concrete, TypeVariable<?> typeArgument) {
        var index = findDeclarationIndex(concrete.type(), typeArgument);
        return concrete.argument(index);
    }

    private static TypeDescriptor mapParameterizedType(ClassDescriptor concrete, ParameterizedType parameterizedType) {
        var arguments = ArrayList.from(mapTypes(concrete, parameterizedType.getActualTypeArguments()));
        var captureStart = arguments.size();

        // this will add all the enclosing type arguments
        addEnclosingElements(arguments, concrete, parameterizedType);

        // by default a type cannot be constant if it has captures.
        var isConstant = captureStart == arguments.size();
        if (isConstant) { // if it hasn't any capture, we still need to check it if uses any type parameter.
            for (var actualTypeArgument : parameterizedType.getActualTypeArguments()) {
                if (actualTypeArgument instanceof TypeVariable<?>) {
                    isConstant = false;
                    break;
                }
            }
        }

        var classDescriptor = ClassDescriptor.ofInternal(
                (Class<?>) parameterizedType.getRawType(),
                captureStart,
                isConstant,
                arguments.toArray(TO_ARRAY)
        );

        if (!classDescriptor.properties().is(Property.CONSTANT)) {
            Analytics.report(classDescriptor, Analytics.Kind.USED);
            return classDescriptor;
        }

        var actual = TypeDescriptorCaching.cache(classDescriptor);
        Analytics.report(classDescriptor, classDescriptor == actual ? Analytics.Kind.USED : Analytics.Kind.DISCARDED);
        return actual;
    }

    private static TypeDescriptor mapArrayType(ClassDescriptor concrete, GenericArrayType type) {
        var componentType = mapType(concrete, type.getGenericComponentType());
        var descriptor = ArrayDescriptor.of(componentType);
        if (!(descriptor instanceof ArrayDescriptor arrayDescriptor)) {
            return descriptor;
        }
        if (!arrayDescriptor.properties().is(Property.CONSTANT)) {
            Analytics.report(arrayDescriptor, Analytics.Kind.USED);
            return descriptor;
        }
        var actual = TypeDescriptorCaching.cache(arrayDescriptor);
        Analytics.report(actual, actual == arrayDescriptor ? Analytics.Kind.USED : Analytics.Kind.DISCARDED);
        return actual;
    }

    private static TypeDescriptor mapClass(Class<?> type) {
        // This handles raw typee
        var result = type.getTypeParameters().length > 0 ? ClassDescriptor.ofRawInternal(type) : ClassDescriptor.ofInternal(type);
        // here we do not need to report anything to analytics, the caching class will do it for us
        var actual = TypeDescriptorCaching.cache(result);
        Analytics.report(result, result == actual ? Analytics.Kind.USED : Analytics.Kind.DISCARDED);
        return actual;
    }

    private static TypeDescriptor mapWildcard() {
        return ErasedClassDescriptor.instance();
    }

    private static void addEnclosingElements(
            ArrayList<TypeDescriptor> arguments,
            ClassDescriptor concrete,
            Type type
    ) {
        Class<?> asClass;
        switch (type) {
            case Class<?> clazz:
                asClass = clazz;
                break;
            case ParameterizedType parameterizedType:
                asClass = (Class<?>) parameterizedType.getRawType();
                break;
            default:
                var message = Utils.join("Unexpected type: ", type);
                throw new AssertionError(message);
        }
        addNextEnclosingElement(arguments, concrete, type, asClass);
    }

    private static void addEnclosingClass(
            ArrayList<TypeDescriptor> arguments,
            ClassDescriptor concrete,
            Type type
    ) {
        Class<?> asClass;
        switch (type) {
            case Class<?> clazz:
                for (var typeParameter : clazz.getTypeParameters()) {
                    var index = findDeclarationIndex(concrete.type(), typeParameter);
                    arguments.add(concrete.argument(index));
                }
                asClass = clazz;
                break;
            case ParameterizedType parameterizedType:
                for (var typeArgument : parameterizedType.getActualTypeArguments()) {
                    arguments.add(mapType(concrete, typeArgument));
                }
                asClass = (Class<?>) parameterizedType.getRawType();
                break;
            default:
                var message = Utils.join("Unexpected type: ", type);
                throw new AssertionError(message);
        }
        addNextEnclosingElement(arguments, concrete, type, asClass);
    }

    private static void addNextEnclosingElement(
            ArrayList<TypeDescriptor> arguments,
            ClassDescriptor concrete,
            Type type,
            Class<?> current
    ) {
        if (current.accessFlags().contains(AccessFlag.STATIC)) return;

        var enclosingMethod = ReflectionUtils.enclosingMethod(current);
        if (enclosingMethod != null) {
            addEnclosingMethod(arguments, concrete, enclosingMethod);
            if (enclosingMethod.accessFlags().contains(AccessFlag.STATIC)) return;
        }

        // Special case for classes defined in blocks or field initializers
        if (enclosingMethod == null && !current.isMemberClass()) {
            var annotation = current.getAnnotation(NestedClassMetadata.class);
            if (annotation == null || annotation.isStatic()) return;
        }

        if (current.getEnclosingClass() != null) {
            if ((type instanceof ParameterizedType parameterizedType) && parameterizedType.getOwnerType() != null) {
                addEnclosingClass(arguments, concrete, parameterizedType.getOwnerType());
            } else {
                addEnclosingClass(arguments, concrete, current.getEnclosingClass());
            }
        }
    }

    private static void addEnclosingMethod(
            ArrayList<TypeDescriptor> arguments,
            ClassDescriptor concrete,
            Executable executable
    ) {
        for (var typeParameter : executable.getTypeParameters()) {
            var index = findDeclarationIndex(concrete.type(), typeParameter);
            arguments.add(concrete.argument(index));
        }
    }

    //endregion

    //region index resolution

    private static int findDeclarationIndex(Class<?> start, TypeVariable<?> argument) {
        var result = findDeclarationIndexInClass(0, start, argument);
        if (result == -1) {
            var message = Utils.join("Cannot find ", argument, " in ", start);
            throw new AssertionError(message);
        }
        return result;
    }

    private static int findDeclarationIndexInClass(int currentIndex, Class<?> current, TypeVariable<?> argument) {
        var params = current.getTypeParameters();
        for (int i = 0; i < params.length; i++) {
            if (params[i] == argument) {
                return currentIndex + i;
            }
        }
        if (current.accessFlags().contains(AccessFlag.STATIC)) return -1;

        var newIndex = currentIndex + params.length;
        if (current.getEnclosingMethod() != null) {
            return findDeclarationIndexInMethod(newIndex, current.getEnclosingMethod(), argument);
        }
        if (current.getEnclosingConstructor() != null) {
            return findDeclarationIndexInMethod(newIndex, current.getEnclosingConstructor(), argument);
        }
        // this should be after the enclosing method checks, as this can return a class even if we are directly inside
        // a method, while the opposite is not true.
        if (current.getEnclosingClass() != null) {
            return findDeclarationIndexInClass(newIndex, current.getEnclosingClass(), argument);
        }

        return -1;
    }

    private static int findDeclarationIndexInMethod(
            int currentIndex,
            Executable current,
            TypeVariable<?> argument
    ) {
        var params = current.getTypeParameters();
        for (int i = 0; i < params.length; i++) {
            if (params[i] == argument) {
                return currentIndex + i;
            }
        }
        var newIndex = currentIndex + params.length;
        if (current.accessFlags().contains(AccessFlag.STATIC)) return -1;
        return findDeclarationIndexInClass(newIndex, current.getDeclaringClass(), argument);
    }

    //endregion

    private static boolean shouldProcess(Class<?> type) {
        return type.isAnnotationPresent(Instrumented.class);
    }

    private static final Equivalence<ClassDescriptor> EXTRACTOR = new Equivalence<>() {
        @Override
        public int hash(ClassDescriptor obj) {
            return obj.type().hashCode();
        }

        @Override
        public boolean equals(ClassDescriptor obj, Object other) {
            if (!(other instanceof ClassDescriptor descriptor)) return false;
            return obj.type().equals(descriptor.type());
        }
    };

    private static final Function<Integer, TypeDescriptor[]> TO_ARRAY = new Function<Integer, TypeDescriptor[]>() {
        @Override
        public TypeDescriptor[] apply(Integer input) {
            return new TypeDescriptor[input];
        }
    };

    private SuperDescriptorComputing() {
        throw new AssertionError();
    }

}
