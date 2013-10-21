libraries/utf8-string_PACKAGE = utf8-string
libraries/utf8-string_dist-install_GROUP = libraries
$(if $(filter utf8-string,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/utf8-string,dist-boot,0)))
$(if $(filter utf8-string,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/utf8-string,dist-install,1)))
$(if $(filter utf8-string,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/utf8-string,dist-install,2)))
