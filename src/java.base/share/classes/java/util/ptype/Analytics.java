package java.util.ptype;

import jdk.internal.vm.annotation.Stable;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

final class Analytics {

    @Stable
    private static int shouldLog;

    public static void report(MethodDescriptor descriptor, Kind kind) {
        report(METHOD_DESCRIPTORS, descriptor, kind);
    }

    public static void report(ClassDescriptor descriptor, Kind kind) {
        report(CLASS_DESCRIPTORS, descriptor, kind);
    }

    public static void report(ArrayDescriptor descriptor, Kind kind) {
        report(ARRAY_DESCRIPTORS, descriptor, kind);
    }

    public enum Kind {
        /// This is the most common kind. A descriptor has been created, and we log from its constructor
        CREATED,
        /// The descriptor will be used, either its a dynamic descriptor, or the first instance of a constant
        /// descriptor.
        USED,
        /// This descriptor will be discarded because its constant and the same descriptor already exists in the cache.
        DISCARDED,
    }

    private static <T> void report(HashMap<T, DescriptorAnalytics<T>> cache, T descriptor, Kind kind) {
        if (shouldLog == -1) return;
        Utils.requireNonNull(descriptor);
        Utils.requireNonNull(kind);
        if (!Status.isBooted()) return;
        if (shouldLog == 0) {
            shouldLog = System.getProperty("genericsAnalytics") != null ? 1 : -1;
            if (shouldLog == 1) {
                registerShutdownHook();
            }
        }
        if (shouldLog == -1) {
            return;
        }
        synchronized (CLASS_DESCRIPTORS) {
            var analytics = cache.computeIfAbsent(descriptor, mapper());
            analytics.update(kind);
        }
    }

    private static final class DescriptorAnalytics<T> {
        @Stable
        private final T descriptor;
        /// We actually don't need this as created = used + discarded, it's just here to keep track of the invariant in
        /// case we didn't add a call to used or discarded
        private int created;
        private int used;
        private int discarded;

        private DescriptorAnalytics(T descriptor) {
            this.descriptor = descriptor;
        }

        public void update(Kind kind) {
            switch (kind) {
                case CREATED:
                    created++;
                    break;
                case USED:
                    used++;
                    break;
                case DISCARDED:
                    discarded++;
                    break;
            }
        }

        @Override
        public String toString() {
            return descriptor.toString();
        }
    }

    private static class Writer {

        private final OutputStream stream;

        private Writer(OutputStream stream) {
            this.stream = stream;
        }

        private void write(Object obj) {
            try {
                stream.write(obj.toString().getBytes(StandardCharsets.UTF_8));
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }

        private void w(Object obj) {
            write(obj);
            write(";");
        }

        private void wln(Object obj) {
            write(obj);
            write("\n");
        }

        private void logResult() {
            logClassDescriptors();
        }

        private void logClassDescriptors() {
            wln("####################################### CLASS DESCRIPTORS #######################################");
            wln("Descriptor;Created;Used;Discarded;Raw;Full;Constant;Arguments Count;Type Arguments Count;Captured Arguments Count");

            for (var it = CLASS_DESCRIPTORS.iterator(); it.hasNext(); ) {
                var analytics = it.next();
                var classDescriptor = analytics.descriptor;

                if (analytics.created != analytics.used + analytics.discarded) {
                    var message = Utils.join(
                            classDescriptor,
                            " ", analytics.created,
                            " != ", analytics.used,
                            " + ", analytics.discarded,
                            " (= ", analytics.used + analytics.discarded, ")"
                    );
                    System.err.println(message);
                }
                w(classDescriptor);
                w(analytics.created);
                w(analytics.used);
                w(analytics.discarded);
                w(classDescriptor.isRaw());
                w(classDescriptor.properties().is(TypeDescriptor.Properties.Property.FULL));
                w(classDescriptor.properties().is(TypeDescriptor.Properties.Property.CONSTANT));
                w(classDescriptor.argumentsCount());
                w(classDescriptor.typeArgumentsCount());
                wln(classDescriptor.capturedTypeArgumentsCount());
            }
        }

    }

    private static void registerShutdownHook() {
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            var path = System.getProperty("genericsAnalyticsFile");
            try (var outputStream = path == null ? System.out : Files.newOutputStream(Path.of(path))) {
                var stream = new BufferedOutputStream(outputStream, BUFFER_SIZE);
                synchronized (CLASS_DESCRIPTORS) {
                    new Writer(stream).logResult();
                }
            } catch (IOException e) {
                // silently close
            }
        }));
    }

    @SuppressWarnings("unchecked")
    private static <T> Function<T, DescriptorAnalytics<T>> mapper() {
        return (Function<T, DescriptorAnalytics<T>>) TO_ANALYTICS;
    }

    // We use custom equivalence to take props into account, which differs from the usual.

    private static final Equivalence<ClassDescriptor> CLASS_DESCRIPTOR_EQUIVALENCE = new Equivalence<>() {
        @Override
        public int hash(ClassDescriptor obj) {
            Utils.requireNonNull(obj);
            var h = 1;
            h = 31 * h + obj.type().hashCode();
            h = 31 * h + obj.capturedTypeArgumentsStartIndex();
            h = 31 * h + obj.properties().hashCode();
            h = 31 * h + Utils.arrayHashCode(obj.arguments(), TYPE_DESCRIPTOR_EQUIVALENCE);
            return h;
        }

        @Override
        public boolean equals(ClassDescriptor obj, Object other) {
            if (!(other instanceof ClassDescriptor classDescriptor)) return false;
            return obj.type().equals(classDescriptor.type())
                    && obj.capturedTypeArgumentsStartIndex() == classDescriptor.capturedTypeArgumentsStartIndex()
                    && obj.properties().equals(classDescriptor.properties())
                    && Utils.arrayEquals(obj.arguments(), classDescriptor.arguments(), TYPE_DESCRIPTOR_EQUIVALENCE);
        }
    };

    private static final Equivalence<ArrayDescriptor> ARRAY_DESCRIPTOR_EQUIVALENCE = new Equivalence<>() {

        @Override
        public int hash(ArrayDescriptor obj) {
            Utils.requireNonNull(obj);
            return TYPE_DESCRIPTOR_EQUIVALENCE.hash(obj.componentType());
        }

        @Override
        public boolean equals(ArrayDescriptor obj, Object other) {
            Utils.requireNonNull(obj);
            if (!(other instanceof ArrayDescriptor arrayDescriptor)) return false;
            return TYPE_DESCRIPTOR_EQUIVALENCE.equals(obj.componentType(), arrayDescriptor.componentType());
        }
    };

    private static final Equivalence<TypeDescriptor> TYPE_DESCRIPTOR_EQUIVALENCE = new Equivalence<>() {

        @Override
        public int hash(TypeDescriptor obj) {
            Utils.requireNonNull(obj);
            switch (obj) {
                case ArrayDescriptor arrayDescriptor:
                    return ARRAY_DESCRIPTOR_EQUIVALENCE.hash(arrayDescriptor);
                case ClassDescriptor classDescriptor:
                    return CLASS_DESCRIPTOR_EQUIVALENCE.hash(classDescriptor);
                case ErasedClassDescriptor erasedClassDescriptor:
                    return erasedClassDescriptor.hashCode();
            }
        }

        @Override
        public boolean equals(TypeDescriptor obj, Object other) {
            Utils.requireNonNull(obj);
            switch (obj) {
                case ArrayDescriptor arrayDescriptor:
                    return ARRAY_DESCRIPTOR_EQUIVALENCE.equals(arrayDescriptor, other);
                case ClassDescriptor classDescriptor:
                    return CLASS_DESCRIPTOR_EQUIVALENCE.equals(classDescriptor, other);
                case ErasedClassDescriptor erasedClassDescriptor:
                    return erasedClassDescriptor.equals(other);
            }
        }
    };

    private static final Equivalence<MethodDescriptor> METHOD_DESCRIPTOR_EQUIVALENCE = new Equivalence<>() {
        @Override
        public int hash(MethodDescriptor obj) {
            Utils.requireNonNull(obj);
            return Utils.arrayHashCode(obj.arguments(), TYPE_DESCRIPTOR_EQUIVALENCE);
        }

        @Override
        public boolean equals(MethodDescriptor obj, Object other) {
            Utils.requireNonNull(obj);
            if (!(other instanceof MethodDescriptor methodDescriptor)) return false;
            return Utils.arrayEquals(obj.arguments(), methodDescriptor.arguments(), TYPE_DESCRIPTOR_EQUIVALENCE);
        }
    };

    private static final HashMap<ClassDescriptor, DescriptorAnalytics<ClassDescriptor>> CLASS_DESCRIPTORS =
            new HashMap<>(CLASS_DESCRIPTOR_EQUIVALENCE);

    private static final HashMap<ArrayDescriptor, DescriptorAnalytics<ArrayDescriptor>> ARRAY_DESCRIPTORS =
            new HashMap<>(ARRAY_DESCRIPTOR_EQUIVALENCE);

    private static final HashMap<MethodDescriptor, DescriptorAnalytics<MethodDescriptor>> METHOD_DESCRIPTORS =
            new HashMap<>(METHOD_DESCRIPTOR_EQUIVALENCE);

    private static final Function<?, ?> TO_ANALYTICS = new Function<Object, DescriptorAnalytics<?>>() {
        @Override
        public DescriptorAnalytics<?> apply(Object input) {
            Utils.requireNonNull(input);
            return new DescriptorAnalytics<>(input);
        }
    };

    private static final int BUFFER_SIZE = 2 << 11;

    private Analytics() {
        throw new AssertionError();
    }
}
