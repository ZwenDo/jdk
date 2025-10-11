package java.util.ptype;

import jdk.internal.misc.VM;
import jdk.internal.vm.annotation.Stable;

/// Utility class to check the VM status.
public final class Status {

    @Stable
    private static boolean isBooted;

    /// Whether the VM is booted.
    ///
    /// @return true if the VM is booted, false otherwise
    public static boolean isBooted() {
        return isBooted || (isBooted = VM.isBooted());
    }

    private Status() {
        throw new AssertionError();
    }

}
