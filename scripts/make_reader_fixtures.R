# Builds two invented test fixtures for the reader repair (0.3.1) from the
# synthetic example: (1) a file that declares UTF-8 but has a windows-1252 byte
# in the header, (2) a file that starts with the literal text "<U+FEFF>".
# Nothing from a real file is used.
root <- local({ d <- getwd(); while (!file.exists(file.path(d, "R", "helpers_paths.R"))) d <- dirname(d); d })
fx <- file.path(root, "inst", "extdata", "inputs", "test_fixtures")
src <- readBin(file.path(fx, "hpr_example_synthetic.hpr"), "raw", file.size(file.path(fx, "hpr_example_synthetic.hpr")))
txt <- rawToChar(src)
# (1) a made-up name with "ø" in windows-1252 (byte 0xF8) in a comment line of the header
decl_end <- regexpr("?>", txt, fixed = TRUE) + 1L
line <- charToRaw("\n<!-- Skogsmaskin Bj\xf8rn (synthetic) -->")
Encoding(txt) <- "bytes"
b <- charToRaw(txt)
out1 <- c(b[seq_len(decl_end)], line, b[(decl_end + 1L):length(b)])
writeBin(out1, file.path(fx, "hpr_cp1252_synthetic.hpr"))
# (2) literal marker text before the XML declaration
writeBin(c(charToRaw("<U+FEFF>"), b), file.path(fx, "hpr_marker_synthetic.hpr"))
