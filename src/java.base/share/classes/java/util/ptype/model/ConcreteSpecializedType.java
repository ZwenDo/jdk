package java.util.ptype.model;


import jdk.internal.vm.annotation.Stable;

import java.util.ptype.Internal;
import java.util.ptype.util.HashMap;

sealed abstract class ConcreteSpecializedType permits ClassType, ParameterizedType, RawType {

//    @Stable
    private HashMap<Class<?>, SpecializedType> superTypes = null;

    protected ConcreteSpecializedType() {
    }

    public final SpecializedType asSuper(Class<?> type) {
        if (superTypes == null) {
            superTypes = Internal.generateSuperTypes(type, (SpecializedType) this);
        }
        return superTypes.get(type);
    }

}
