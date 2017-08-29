all:
	cabal install \
		--bindir=./ \
		--enable-profiling \
		--enable-benchmarks

.PHONY: fast
fast:
	cabal install --bindir=./ -O2

.PHONY: run
run:
	cabal run

# The poorly named "profile" command runs all tests, benchmarks, and whatnot
# and gathers the results into an archive, along with time and git info.
# It first builds the dir "000_profile_<timestamp>/", which stores:
#  * html benchmark report from Criterion
#  * several memory profiles
#  * git info of current commit and status
#  * results of tests (not done yet)
#  * test coverage (not done yet)
#  * documentation coverage (not done yet)
.PHONY: profile
profile:
	./profile.sh

.PHONY: clean
clean:
	rm -f *.aux *.ps *.hp *.prof

.PHONY: deepclean
deepclean:
	${MAKE} clean
	rm -rf fagin dist
