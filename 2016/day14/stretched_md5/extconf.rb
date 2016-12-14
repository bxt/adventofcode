require 'mkmf'

extension_name = 'stretched_md5'

dir_config("openssl")
pkg_config("openssl")

have_library("crypto") && have_header("openssl/md5.h")

dir_config(extension_name)

create_makefile(extension_name)
