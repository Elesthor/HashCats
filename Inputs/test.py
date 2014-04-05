def readFile(path):
    f=open(path,"r")
    (nintersections,streets, time, cars, start )=map(int, f.readline().split())
    print(nintersections)
    print(streets)
    print(time)
    print(cars)
    print(start)
    intersection={(i,tuple((map(float, f.readline().split())))) for i in range(nintersections)}
    print(intersection)
                  
#def writeFile:
    



readFile("doodle.txt")



