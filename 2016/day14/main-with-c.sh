#1/bin/sh

cd stretched_md5
ruby extconf.rb
make
cd ..
ruby main.rb --use-native-extension
