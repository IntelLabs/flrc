MLTON_SRC_URL="https://sourceforge.net/projects/mlton/files/mlton/20130715/mlton-20130715.src.tgz/download"
MLTON_BIN=$(shell which mlton)
MLTON_LIBDIR=$(abspath $(dir $(MLTON_BIN))/../lib/sml)
DIR_LIST=basis ckit-lib cml mllpt-lib mlnlffi-lib mlrisc-lib mlyacc-lib smlnj-lib

mlb-path-map: mlton $(DIR_LIST)
	@echo "MLTON_ROOT $(shell pwd)" > mlb-path-map
	@echo "SML_LIB $(shell pwd)" >> mlb-path-map

./mlton:
	@echo Downloading mlton source and extract its directory lib/mlton to ./mlton.
	curl -L $(MLTON_SRC_URL)|tar zxf - mlton-20130715/lib/mlton --strip-components=2

$(DIR_LIST):
	@echo linking $@ from system mlton installation to ./$@
	@ln -s "$(MLTON_LIBDIR)/$@" .

clean:
	rm -f $(DIR_LIST)

cleanall: clean
	rm -rf mlton
