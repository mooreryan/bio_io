# Assumes you're running from the project root.
mkdir -p fuzz/afl_output/ic

afl-fuzz -i fuzz/test_files/ic \
	 -o fuzz/afl_output/ic \
	 -S fuzzer3 \
	 _build/default/fuzz/fuzz_in_channel.exe @@
