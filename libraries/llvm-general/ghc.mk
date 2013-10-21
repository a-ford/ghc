libraries/llvm-general_PACKAGE = llvm-general
libraries/llvm-general_dist-install_GROUP = libraries
$(if $(filter llvm-general,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/llvm-general,dist-boot,0)))
$(if $(filter llvm-general,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/llvm-general,dist-install,1)))
$(if $(filter llvm-general,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/llvm-general,dist-install,2)))
