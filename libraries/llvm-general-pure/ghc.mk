libraries/llvm-general-pure_PACKAGE = llvm-general-pure
libraries/llvm-general-pure_dist-install_GROUP = libraries
$(if $(filter llvm-general-pure,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/llvm-general-pure,dist-boot,0)))
$(if $(filter llvm-general-pure,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/llvm-general-pure,dist-install,1)))
$(if $(filter llvm-general-pure,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/llvm-general-pure,dist-install,2)))
