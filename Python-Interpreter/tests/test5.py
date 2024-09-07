i = 2;

def main(x=5):
    global i;
    i = i+1;
    print(i*x);
    return x;;

print(main());
print(i);