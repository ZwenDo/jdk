package java.util.ptype;

import jdk.internal.vm.annotation.Stable;

import java.lang.invoke.MethodHandle;
import java.util.Optional;
import java.util.ptype.util.HashMap;
import java.util.ptype.util.HashSet;
import java.util.ptype.util.Utils;

/// Descriptor for a lambda
final class LambdaDescriptor implements DerivableDescriptor {

    @Stable
    private final SpecializedTypeDescriptor[] flattenedTypeArguments;

    @Stable
    private final MethodHandle computeSuperMethod;

    @Stable
    private HashMap<Class<?>, ClassDescriptor> superTypes = null;

    private LambdaDescriptor(
            int dummy,
            MethodHandle computeSuperMethod,
            SpecializedTypeDescriptor[] flattenedTypeArguments
    ) {
        this.computeSuperMethod = computeSuperMethod;
        this.flattenedTypeArguments = flattenedTypeArguments;
    }

    public LambdaDescriptor(MethodHandle computeSuperMethod, SpecializedTypeDescriptor arg1) {
        Utils.requireNonNull(computeSuperMethod);
        checkMethodHandle(computeSuperMethod);
        Utils.requireNonNull(arg1);
        this(0, computeSuperMethod, new SpecializedTypeDescriptor[]{arg1});
    }

    public LambdaDescriptor(
            MethodHandle computeSuperMethod,
            SpecializedTypeDescriptor arg1,
            SpecializedTypeDescriptor arg2
    ) {
        Utils.requireNonNull(computeSuperMethod);
        checkMethodHandle(computeSuperMethod);
        Utils.requireNonNull(arg1);
        Utils.requireNonNull(arg2);
        this(0, computeSuperMethod, new SpecializedTypeDescriptor[]{arg1, arg2});
    }

    public LambdaDescriptor(
            MethodHandle computeSuperMethod,
            SpecializedTypeDescriptor arg1,
            SpecializedTypeDescriptor arg2,
            SpecializedTypeDescriptor arg3
    ) {
        Utils.requireNonNull(computeSuperMethod);
        checkMethodHandle(computeSuperMethod);
        Utils.requireNonNull(arg1);
        Utils.requireNonNull(arg2);
        Utils.requireNonNull(arg3);
        this(0, computeSuperMethod, new SpecializedTypeDescriptor[]{arg1, arg2, arg3});
    }

    public LambdaDescriptor(
            MethodHandle computeSuperMethod,
            SpecializedTypeDescriptor... flattenedTypeArguments
    ) {
        Utils.requireNonNull(computeSuperMethod);
        checkMethodHandle(computeSuperMethod);
        Utils.requireNonNull(flattenedTypeArguments);
        var copy = new SpecializedTypeDescriptor[flattenedTypeArguments.length];
        for (int i = 0; i < flattenedTypeArguments.length; i++) {
            copy[i] = Utils.requireNonNull(flattenedTypeArguments[i]);
        }
        this(0, computeSuperMethod, copy);
    }

    @Override
    public Optional<ClassDescriptor> asSuper(Class<?> type) {
        Utils.requireNonNull(type);
        if (superTypes == null) {
            var set = new HashSet<SuperTypeMapping>();
            try {
                computeSuperMethod.invokeExact(flattenedTypeArguments, set);
            } catch (Throwable e) {
                throw new RuntimeException(e);
            }
            superTypes = HashMap.superTypeMap(set);
        }
        return Optional.ofNullable(superTypes.get(type));
    }

    private static void checkMethodHandle(MethodHandle computeSuperMethod) {
        var type = computeSuperMethod.type();
        if (type.returnType() != void.class) {
            throw new IllegalArgumentException("The method should not return any value.");
        }
        if (type.parameterCount() != 2) {
            throw new IllegalArgumentException("The method should only have two parameters.");
        }
        if (type.parameterType(0) != ClassDescriptor[].class) {
            throw new IllegalArgumentException("The first parameter should be an array of ClassDescriptor");
        }
        if (type.parameterType(1) != HashSet[].class) {
            throw new IllegalArgumentException("The second parameter should be an HashSet");
        }
    }

}
