package java.util.ptype;

import jdk.internal.misc.VM;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.ptype.model.*;
import java.util.ptype.util.Predicate;
import java.util.ptype.util.Utils;


/// Class providing operations on [SpecializedTypes][SpecializedType].
public final class SpecializedTypeUtils {

    //region Stringify

    /// Return the [String] representation of a given [SpecializedType].
    ///
    /// @param type the specialized type we want the string representation of
    /// @return the string representation of the specialized type
    public static String stringify(SpecializedType type) {
        Utils.requireNonNull(type);
        var builder = new StringBuilder();
        appendToBuilder(builder, type);
        return builder.toString();
    }

    /// Appends the string representation of a [SpecializedType] to a [StringBuilder]
    ///
    /// @param builder the builder
    /// @param type the specialized type
    public static void appendToBuilder(StringBuilder builder, SpecializedType type) {
        Utils.requireNonNull(builder);
        switch (type) {
            case ArrayType arrayType:
                appendToBuilder(builder, arrayType.componentType());
                builder.append("[]");
                break;
            case ClassType classType:
                builder.append(classType.type().getSimpleName());
                break;
            case InnerClassType innerClassType:
                appendToBuilder(builder, innerClassType.outerType());
                builder.append('.');
                appendToBuilder(builder, innerClassType.innerType());
                break;
            case IntersectionType intersection:
                intersection.bounds()
                        .joinTo(builder, SpecializedTypeUtils::appendToBuilder, " & ");
                break;
            case ParameterizedType parameterizedType:
                builder.append(parameterizedType.rawType().getSimpleName());
                builder.append("<");
                parameterizedType.actualTypeArguments()
                        .joinTo(builder, SpecializedTypeUtils::appendToBuilder, ", ");
                builder.append(">");
                break;
            case RawType rawType:
                builder.append(rawType.type().getSimpleName());
                break;
            case WildcardType wildcard:
                builder.append('?');
                if (wildcard.isSuper()) {
                    builder.append(" super ");
                    wildcard.superBound().joinTo(builder, SpecializedTypeUtils::appendToBuilder, " & ");
                } else if (!wildcard.extendsBound().isEmpty()) {
                    wildcard.extendsBound().joinTo(builder, SpecializedTypeUtils::appendToBuilder, " & ");
                }
                var kind = wildcard.isSuper() ? " super " : " extends ";
                builder.append(kind);
                break;
        }
    }

    //endregion

    //region Type Extraction

    /// Extracts a specific [SpecializedType] contained in a [SpecializedTypeContainer].
    ///
    /// Each index in the `indices` argument represent a level of nesting in the `container` specialized type.
    ///
    /// Here is an example of how this method works:
    ///
    /// given the type `type` representing `List<Function<String, Integer>>`, the following call `extract(type, 0)` will
    /// return `Function<String, Integer>`. The call `extract(type, 0, 1)` will yield `Integer`
    ///
    /// @param container the container in which we want to extract information
    /// @param indices the indices of the information in the container, each element representing a new level of nesting
    /// @return the extracted [SpecializedType]
    public static SpecializedType extract(SpecializedTypeContainer container, int... indices) {
        Utils.requireNonNull(container);
        Utils.requireNonNull(indices);
        if (indices.length == 0) {
            throw new IllegalArgumentException("indices.length == 0");
        }
        switch (container) {
            case SpecializedMethodTypeArguments mta:
                return extract(mta.typeArgument(indices[0]), 1, indices);
            case SpecializedType specializedType:
                return extract(specializedType, 0, indices);
            default:
                throw new AssertionError("Unexpected value: " + container);
        }
    }

    private static SpecializedType extract(SpecializedType type, int start, int[] indices) {
        var currentType = type;
        var currentIndex = start;

        while (currentIndex < indices.length) {
            switch (currentType) {
                case ParameterizedType p:
                    currentType = p.actualTypeArguments().get(indices[currentIndex]);
                    currentIndex++;
                    break;
                case RawType rt:
                    throw new NotImplementedException();
//                    currentType = rawTypeArg(rt.type());
//                    break;
                case ArrayType a:
                    currentType = a.componentType();
                    break;
                case InnerClassType i:
                    currentType = i.innerType();
                    break;
                case WildcardType _:
                case IntersectionType _:
                case ClassType _:
                    throw new IllegalArgumentException(currentType.getClass().getSimpleName() + " not supported");
                default:
                    throw new AssertionError("Unexpected value: " + currentType);
            }
        }

        return currentType;
    }
    //endregion

    //region Type Verification

    /// Tests whether a given object has the expected [SpecializedType] and returns it. This method will print an error
    /// if the `obj` is not a subtype of the `expected` specialized type.
    ///
    /// @param obj the object to test
    /// @param expected the expected type
    /// @return the object
    public static Object checkCast(
            Object obj,
            SpecializedType expected
    ) {
        Utils.requireNonNull(expected);
        if (!VM.isBooted()) return obj;

        if (obj == null) return null;

        if (!isInstance(obj, expected)) {
            System.err.println(errorMessage(obj, expected));
        }

        return obj;
    }

    private static boolean isInstance(Object obj, SpecializedType expected) {
        switch (expected) {
            // var cast = (A<String>.B<Integer>) obj;
            case InnerClassType innerClassType:
                if (!isInstance(obj, innerClassType.innerType())) { // check inner type
                    return false;
                }

                // we need to extract the expected outer class, because the actual object inner class might have an outer
                // this that does not extend the expected outer class (e.g. Attr.ResultInfo & Resolve.MethodResultInfo).
                Class<?> expectedOuterClass;
                var outerClassArg = innerClassType.outerType();
                if (outerClassArg instanceof ClassType outerClassType) {
                    expectedOuterClass = outerClassType.type();
                } else if (outerClassArg instanceof ParameterizedType parameterizedType) {
                    expectedOuterClass = parameterizedType.rawType();
                } else if (outerClassArg instanceof RawType rawType) {
                    expectedOuterClass = rawType.type();
                } else {
                    throw new AssertionError("Unexpected outer type: " + innerClassType.outerType());
                }

                var outer = Internal.outerThis(obj, expectedOuterClass);
                if (outer.isPresent()) {
                    return isInstance(outer.get(), innerClassType.outerType());
                } else { // by default if no outer type is specified, yield true
                    return true;
                }

                // var cast = (String) obj; (usually (E) obj;)
            case ClassType classType:
                return classType.type().isAssignableFrom(obj.getClass());

            // var cast = (List) obj;
            case RawType rawType:
                return validate(obj, expected, rawType.type());

            // var cast = (List<String>) obj;
            case ParameterizedType parameterizedType:
                return validate(obj, expected, parameterizedType.rawType());

            // var cast = (Runnable & Serializable) obj;
            case IntersectionType intersection:
                return intersection.bounds().allMatch(new Predicate<>() {
                    @Override
                    public boolean test(SpecializedType bound) {
                        return isInstance(obj, bound);
                    }
                });

            // var cast = (List<String>[]) obj;
            case ArrayType arrayType:
                if (!obj.getClass().isArray()) return false;
                return validate(obj, expected, obj.getClass());

            // Note that this kind of cast should not be possible
            // var cast = (? extends String) obj;
            // var cast = (? super String) obj;
            // var cast = (?) obj; equivalent to (? extends Object)
            case WildcardType wildcard:
                var objClass = obj.getClass();
                var opt = Internal.extractInformationField(obj);
                if (opt.isEmpty()) {
                    return true;
                }
                var actualArg = opt.get();
                return false;
            // TODO
//                return wildcard.superBound().allMatch(Utils.isAssignableLambdaActual(actualArg, Arg.Variance.COVARIANT)) &&
//                        wildcard.extendsBound().allMatch(Utils.isAssignableLambdaActual(actualArg, Arg.Variance.CONTRAVARIANT));
            case null:
            default:
                throw new AssertionError();
        }
    }

    private static boolean validate(Object obj, SpecializedType expected, Class<?> supertype) {
        var objClass = obj.getClass();
        var opt = Internal.extractInformationField(obj);
        if (opt.isEmpty()) {
            return supertype.isAssignableFrom(objClass);
        }
        return isAssignable(expected, opt.get());
    }

    private static String errorMessage(Object obj, SpecializedType expected) {
        var objClass = obj.getClass();
        if (objClass.isAnonymousClass()) {
            var interfaces = objClass.getInterfaces();
            objClass = interfaces.length > 0 ? interfaces[0] : objClass.getSuperclass();
        }

        var builder = new StringBuilder();

        var type = Internal.extractInformationField(objClass);
        if (type.isPresent()) {
            appendToBuilder(builder, type.get());
        } else {
            builder.append(objClass.getName());
        }

        builder.append(" to ");
        appendToBuilder(builder, expected);
        return builder.toString();
    }

    private static boolean isAssignable(SpecializedType expected, SpecializedType actual) {
        return false;
    }
    //endregion

    /// Extracts the specialized type information viewed as one of its supertypes.
    ///
    /// @param obj the object containing the specialized type
    /// @param type the supertype we want to see it as
    /// @return the super specialized type
    public static SpecializedType extractsAsSuper(Object obj, Class<?> type) {
        Utils.requireNonNull(obj);
        Utils.requireNonNull(type);
        var field = Internal.extractInformationField(obj);
        if (field.isEmpty()) {
            return null;
        }
        var specializedType = field.get();
        switch (specializedType) {
            case ClassType classType:
                return classType.asSuper(type);
            case ParameterizedType parameterizedType:
                return parameterizedType.asSuper(type);
            case RawType rawType:
                return rawType.asSuper(type);
            case ArrayType _:
            case InnerClassType _:
            case IntersectionType _:
            case WildcardType _:
                throw new IllegalArgumentException("Invalid type: " + stringify(specializedType));
            default:
                throw new IllegalArgumentException();
        }
    }

    private SpecializedTypeUtils() {
        throw new AssertionError();
    }
}
