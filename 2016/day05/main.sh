gcc -Ofast -Wall main.c -o main -I/usr/local/opt/openssl/include -L/usr/local/opt/openssl/lib -lssl -lcrypto && ./main && rm main
