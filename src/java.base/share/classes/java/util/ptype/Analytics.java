package java.util.ptype;

import jdk.internal.vm.annotation.Stable;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Comparator;

final class Analytics {

    @Stable
    private static int shouldLog;

    public static void reportCreation(MethodDescriptor descriptor) {
        report(METHOD_DESCRIPTORS, descriptor, Kind.CREATED);
    }

    public static void reportUsage(MethodDescriptor descriptor) {
        report(METHOD_DESCRIPTORS, descriptor, Kind.USED);
    }

    public static void reportCaching(MethodDescriptor descriptor, MethodDescriptor cached) {
        reportCaching(METHOD_DESCRIPTORS, descriptor, cached);
    }

    public static void reportCreation(ClassDescriptor descriptor) {
        report(CLASS_DESCRIPTORS, descriptor, Kind.CREATED);
    }

    public static void reportUsage(ClassDescriptor descriptor) {
        report(CLASS_DESCRIPTORS, descriptor, Kind.USED);
    }

    public static void reportCaching(ClassDescriptor descriptor, ClassDescriptor cached) {
        reportCaching(CLASS_DESCRIPTORS, descriptor, cached);
    }

    public static void reportCreation(ArrayDescriptor descriptor) {
        report(ARRAY_DESCRIPTORS, descriptor, Kind.CREATED);
    }

    public static void reportUsage(ArrayDescriptor descriptor) {
        report(ARRAY_DESCRIPTORS, descriptor, Kind.USED);
    }

    public static void reportCaching(ArrayDescriptor descriptor, ArrayDescriptor cached) {
        reportCaching(ARRAY_DESCRIPTORS, descriptor, cached);
    }

    private enum Kind {
        /// This is the most common kind. A descriptor has been created, and we log from its constructor
        CREATED,
        /// The descriptor will be used, either its a dynamic descriptor, or the first instance of a constant
        /// descriptor.
        USED,
        /// This descriptor will be discarded because its constant and the same descriptor already exists in the cache.
        DISCARDED,
    }

    private static <T> void reportCaching(HashMap<T, DescriptorAnalytics<T>> cache, T descriptor, T cached) {
        if (shouldLog == -1) return;
        report(cache, descriptor, descriptor == cached ? Kind.USED : Kind.DISCARDED);
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

        public void logResult() {
            logClassDescriptors();
        }

        private void logClassDescriptors() {
            wln("####################################### CLASS DESCRIPTORS #######################################");
            wln("Descriptor;Created;Used;Discarded;Raw;Full;Constant;Arguments Count;Type Arguments Count;Captured Arguments Count");
            @SuppressWarnings({"unchecked", "rawtypes"})
            var array = (DescriptorAnalytics<ClassDescriptor>[]) new DescriptorAnalytics[CLASS_DESCRIPTORS.size()];
            var i = 0;
            for (var it = CLASS_DESCRIPTORS.iterator(); it.hasNext(); ) {
                array[i++] = it.next();
            }
            Arrays.sort(array, CLASS_DESCRIPTOR_COMPARATOR);

            for (var analytics : array) {
                var classDescriptor = analytics.descriptor;

                if (analytics.created != analytics.used + analytics.discarded) {
                    var message = Utils.join(
                            classDescriptor,
                            " has abnormal usage statistics: ", analytics.created,
                            " != ", analytics.used,
                            " + ", analytics.discarded,
                            " (= ", analytics.used + analytics.discarded, ")"
                    );
                    System.err.println(message);
                }
                if (analytics.used == 0) {
                    var message = Utils.join(
                            classDescriptor,
                            " has never been used: ", analytics.used,
                            " (created = ", analytics.created,
                            ", discarded = ", analytics.discarded,
                            ")"
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

    }

    private static void registerShutdownHook() {
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            var path = System.getProperty("genericsAnalyticsFile");
            synchronized (CLASS_DESCRIPTORS) {
                if (path != null) {
                    try (var stream = new BufferedOutputStream(Files.newOutputStream(Path.of(path)))) {
                        synchronized (CLASS_DESCRIPTORS) {
                            new Writer(stream).logResult();
                        }
                    } catch (IOException e) {
                        // silently close
                    }
                } else {
                    var stream = new BufferedOutputStream(System.out);
                    synchronized (CLASS_DESCRIPTORS) {
                        new Writer(stream).logResult();
                    }
                }
            }
        }));
    }

    @SuppressWarnings("unchecked")
    private static <T> Function<T, DescriptorAnalytics<T>> mapper() {
        return (Function<T, DescriptorAnalytics<T>>) TO_ANALYTICS;
    }

    private static final Comparator<DescriptorAnalytics<ClassDescriptor>> CLASS_DESCRIPTOR_COMPARATOR =
            new Comparator<>() {
                @Override
                public int compare(DescriptorAnalytics<ClassDescriptor> o1, DescriptorAnalytics<ClassDescriptor> o2) {
                    Utils.requireNonNull(o1);
                    Utils.requireNonNull(o2);
                    return o1.descriptor.toString().compareTo(o2.descriptor.toString());
                }
            };

    private static final HashMap<ClassDescriptor, DescriptorAnalytics<ClassDescriptor>> CLASS_DESCRIPTORS =
            new HashMap<>();

    private static final HashMap<ArrayDescriptor, DescriptorAnalytics<ArrayDescriptor>> ARRAY_DESCRIPTORS =
            new HashMap<>();

    private static final HashMap<MethodDescriptor, DescriptorAnalytics<MethodDescriptor>> METHOD_DESCRIPTORS =
            new HashMap<>();

    private static final Function<?, ?> TO_ANALYTICS = new Function<Object, DescriptorAnalytics<?>>() {
        @Override
        public DescriptorAnalytics<?> apply(Object input) {
            Utils.requireNonNull(input);
            return new DescriptorAnalytics<>(input);
        }
    };

    private Analytics() {
        throw new AssertionError();
    }
}
