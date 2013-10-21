libraries/setenv_PACKAGE = setenv
libraries/setenv_dist-install_GROUP = libraries
$(if $(filter setenv,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/setenv,dist-boot,0)))
$(if $(filter setenv,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/setenv,dist-install,1)))
$(if $(filter setenv,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/setenv,dist-install,2)))
