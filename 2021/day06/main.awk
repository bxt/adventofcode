BEGIN{RS=","}{a[$1]++}END{while(j<256)a[(7+j++)%9]+=a[j%9];for(x in a)s+=a[x];print s}

# Run with:
# awk -f main.awk input.txt