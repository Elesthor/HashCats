def readFile(path):
    f=open(path,"r")
    (nintersections,nstreets, time, cars, start )=map(int, f.readline().split())
    print(nintersections)
    print(nstreets)
    print(time)
    print(cars)
    print(start)
    intersection={(i,tuple((map(float, f.readline().split())))) for i in range(nintersections)}
    #print(intersection)
    #streets={(i, tuple(map(int, f.readline().split()))) for i in range(nstreets)}   
    #print(streets)
    streets=[tuple() for i in range(nintersections) for j in range(nintersections)]
    for line in f.readlines():
        
        a=list(map(int, line.split()))
        print(a)
        streets2[a[0]][a[1]]=tuple(a)
    print(streets2)
        
#def writeFile:
    



readFile("doodle.txt")



