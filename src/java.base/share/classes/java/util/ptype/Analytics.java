package java.util.ptype;

import jdk.internal.misc.VM;
import jdk.internal.vm.annotation.Stable;

import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

final class Analytics {

    @Stable
    private static OutputStream stream;

    @Stable
    private static int shouldLog;

    private static final HashSet<DescriptorAnalytics> LOGGED = new HashSet<>(new Function<DescriptorAnalytics, Object>() {
        @Override
        public Object apply(DescriptorAnalytics input) {
            return input.descriptor;
        }
    });

    public static void log(TypeDescriptor created) {
        Utils.requireNonNull(created);
        if (!VM.isBooted()) return;
        if (shouldLog == 0) {
            shouldLog = System.getProperty("genericsAnalytics") != null ? 1 : -1;
            if (shouldLog == 1) {
                registerShutdownHook();
            }
        }
        if (shouldLog == -1) {
            return;
        }
        try {
            initStream();
            logDescriptor(created);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static void logDescriptor(TypeDescriptor descriptor) throws IOException {
        var analytics = LOGGED.computeIfAbsent(descriptor, MAPPER);
        analytics.count++;
        if (analytics.count == 1) {
            return;
        }
        write(descriptor.getClass().getSimpleName());
        write(";");
        write(descriptor.toString());
        write(";");
        write(descriptor.properties().toString());
        write("\n");
    }

    private static void write(String string) throws IOException {
        stream.write(string.getBytes(StandardCharsets.UTF_8));
    }

    private static void initStream() throws IOException {
        if (stream != null) return;
        var path = System.getProperty("genericsAnalyticsFile");
        stream = path == null ? System.out : Files.newOutputStream(Path.of(path));
    }

    private static void logResult() throws IOException {
        write("####################################### FINAL RESULTS #######################################");
        write("Total descriptors: ");
        write(String.valueOf(LOGGED.size()));
        write();
    }

    private static final class DescriptorAnalytics {
        private final Object descriptor;
        private int count;

        private DescriptorAnalytics(TypeDescriptor descriptor) {
            this.descriptor = descriptor;
        }

    }

    private static void registerShutdownHook() {
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            try {
                logResult();
                stream.close();
            } catch (IOException e) {
                // silently close
            }
        }));
    }

    private static final Function<TypeDescriptor, DescriptorAnalytics> MAPPER = new Function<>() {
        @Override
        public DescriptorAnalytics apply(TypeDescriptor input) {
            return new DescriptorAnalytics(input);
        }
    };

    private Analytics() {
        throw new AssertionError();
    }
}
