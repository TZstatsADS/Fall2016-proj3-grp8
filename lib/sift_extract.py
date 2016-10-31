import numpy as np
import cv2
import glob
from os import path
import pdb



image_names = np.array(glob.glob("Images/*.jpg"))
print(image_names[:5])

#image_names = ['images/image1.jpg', 'images/image2.jpg']

def SIFT_feature(img_list):
    sift = cv2.xfeatures2d.SIFT_create()
    des_list = []
    for i in range(len(img_list)):
        image = cv2.imread(img_list[i])
        gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
        res = cv2.resize(gray,(256,256),interpolation = cv2.INTER_LINEAR)
        (kp, desc) = sift.detectAndCompute(res,None)
        label = np.empty(desc.shape[0])
        label.fill(i)
        desc = np.column_stack((desc, label))
        des_list.append(desc)
    return des_list
ft_img = SIFT_feature(image_names)

print(ft_img[:5])
len(ft_img)

with file("raw_sift_features.txt", "w") as outfile:
    for data_slice in ft_img:
        np.savetxt(outfile,data_slice,fmt='%-7.2f')

# np.savetxt("raw_sift_features.csv",ft_img,delimiter=",")
