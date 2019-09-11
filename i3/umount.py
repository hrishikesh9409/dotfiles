import os
import getpass 

username = getpass.getuser()

os.system("ls | grep mount")
drive_to_be_umounted = input("mount# to be umounted (enter dir #): ")
os.system("sudo umount mount"+str(drive_to_be_umounted))
os.rmdir("/home/"+username+"/mount"+str(drive_to_be_umounted))
