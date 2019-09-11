import os
import getpass 

username = getpass.getuser()

os.system("sudo fdisk -l")
drive_to_be_mounted=raw_input("enter drive to be mounted: ")

dir_no=0
for i in range(0, 10):
    if(os.path.isdir("/home/"+username+"/mount"+str(i))):
        pass
    else:
        os.mkdir("/home/"+username+"/mount"+str(i))
        dir_no=i
        break

print("sudo mount /dev/"+drive_to_be_mounted+" /home/"+username+"/mount"+str(dir_no))
os.system("sudo mount /dev/"+drive_to_be_mounted+" /home/"+username+"/mount"+str(dir_no))
