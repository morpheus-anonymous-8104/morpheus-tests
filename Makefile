.PHONY: clean

clean:
	make -C gproc_test distclean
	make -C locks_test distclean
	make -C mnesia_test distclean
	make -C rabbit_test distclean
	make -C ra_test distclean
	make -C morpheus_test_lib distclean
