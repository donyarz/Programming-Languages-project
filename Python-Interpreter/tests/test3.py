status = 0;

def scope(status = 1):
    print(status);
    return status + 1;;

def main():
    print(status);
    y = [1,2,3,4,5];
    x = scope();
    y2 = y[0];
    return x - y2;;

x = main();
print(x);
print(main());
    