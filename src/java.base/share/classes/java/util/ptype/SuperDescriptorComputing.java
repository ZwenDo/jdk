package java.util.ptype;

import java.lang.reflect.*;

final class SuperDescriptorComputing {

    static HashMap<Class<?>, ClassDescriptor> buildSuperMap(ClassDescriptor concrete) {
        Utils.requireNonNull(concrete);
        var set = buildRegularClassMap(concrete);
        return HashMap.superTypeMap(set);
    }

    static HashMap<Class<?>, ClassDescriptor> buildSuperMap(HiddenClassDescriptor concrete) {
        Utils.requireNonNull(concrete);
        return buildHiddenClassMap(concrete);
    }

    private static HashMap<Class<?>, ClassDescriptor> buildHiddenClassMap(HiddenClassDescriptor concrete) {
        var finalSet = new HashSet<>(EXTRACTOR);
        concrete.forEachDirectSuperType(new BiConsumer<ClassDescriptor, Boolean>() {
            @Override
            public void accept(ClassDescriptor concrete, Boolean ignored) {
                finalSet.addAll(buildRegularClassMap(concrete));
            }
        });
        return HashMap.superTypeMap(finalSet);
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

            superDescriptor.superTypes = HashMap.superTypeMap(mappings);
            allMappings.add(superDescriptor);
        }

        if (current.getGenericSuperclass() != null && shouldProcess(current.getSuperclass())) {
            var superDescriptor = (ClassDescriptor) mapType(concrete, current.getGenericSuperclass());
            var mappings = buildRegularClassMap(superDescriptor);
            allMappings.addAll(mappings);

            // only add if the type is parameterized
            if (superDescriptor.hasArgument()) {
                superDescriptor.superTypes = HashMap.superTypeMap(mappings);
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
                throw new AssertionError("Unexpected type " + typeArgument);
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
        addEnclosing(arguments, concrete, parameterizedType);

        var classDescriptor = ClassDescriptor.of(
                (Class<?>) parameterizedType.getRawType(),
                captureStart,
                arguments.toArray(TO_ARRAY)
        );

        return classDescriptor.properties().isConstant()
                ? TypeDescriptorCaching.cache(classDescriptor)
                : classDescriptor;
    }

    private static TypeDescriptor mapArrayType(ClassDescriptor concrete, GenericArrayType type) {
        var componentType = mapType(concrete, type.getGenericComponentType());
        var arrayDescriptor = ArrayDescriptor.of(componentType);
        if (arrayDescriptor.properties().isConstant()) {
            arrayDescriptor = TypeDescriptorCaching.cache((ArrayDescriptor) arrayDescriptor);
        }
        return arrayDescriptor;
    }

    private static TypeDescriptor mapClass(Class<?> type) {
        return TypeDescriptorCaching.cache(ClassDescriptor.of(type));
    }

    private static TypeDescriptor mapWildcard() {
        return ErasedClassDescriptor.instance();
    }

    private static void addEnclosing(
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
                throw new AssertionError("Unexpected type: " + type);
        }

        if (asClass.getEnclosingMethod() != null) {
            addEnclosingMethod(arguments, concrete, asClass.getEnclosingMethod());
        }
        if (asClass.getEnclosingConstructor() != null) {
            addEnclosingMethod(arguments, concrete, asClass.getEnclosingConstructor());
        }

        if ((type instanceof ParameterizedType parameterizedType) && parameterizedType.getOwnerType() != null) {
            addEnclosing(arguments, concrete, parameterizedType.getOwnerType());
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
            throw new AssertionError("Cannot find " + argument + " in " + start);
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
        var newIndex = currentIndex + params.length;
        if (current.getEnclosingMethod() != null) {
            return findDeclarationIndexInExecutable(newIndex, current.getEnclosingMethod(), argument);
        }
        if (current.getEnclosingConstructor() != null) {
            return findDeclarationIndexInExecutable(newIndex, current.getEnclosingConstructor(), argument);
        }
        // this should be after the enclosing method checks, as this can return a class even if we are directly inside
        // a method, while the opposite is not true.
        if (current.getEnclosingClass() != null) {
            return findDeclarationIndexInClass(newIndex, current.getEnclosingClass(), argument);
        }

        return -1;
    }

    private static int findDeclarationIndexInExecutable(int currentIndex, Executable current, TypeVariable<?> argument) {
        var params = current.getTypeParameters();
        for (int i = 0; i < params.length; i++) {
            if (params[i] == argument) {
                return currentIndex + i;
            }
        }
        var newIndex = currentIndex + params.length;
        return findDeclarationIndexInClass(newIndex, current.getDeclaringClass(), argument);
    }

    //endregion

    private static Class<?> typeToClass(Type type) {
        switch (type) {
            case Class<?> clazz:
                return clazz;
            case ParameterizedType parameterizedType:
                return typeToClass(parameterizedType.getRawType());
            default:
                throw new AssertionError("Unexpected type: " + type);
        }
    }

    private static boolean shouldProcess(Class<?> type) {
        return type.isAnnotationPresent(Instrumented.class);
    }

    private static final Function<ClassDescriptor, Object> EXTRACTOR = new Function<ClassDescriptor, Object>() {
        @Override
        public Object apply(ClassDescriptor input) {
            return input.type();
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
