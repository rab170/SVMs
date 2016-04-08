#!/usr/bin/python

import os
import glob
import numpy as np
import matplotlib.pyplot as plt
from skimage.color import rgb2grey
from skimage.transform import resize
from skimage.io import imread, imsave
from skimage.filters import threshold_otsu

def rgb_to_binary(img):
    grey = rgb2grey(img)
    thresh = threshold_otsu(grey)
    bw = grey >= thresh
    return normalize_background(bw)

def normalize_background(img):
    """
        edge checking to normalize BW values of letter/background
    """
    top = img[0, :]
    bottom = img[-1, :]
    left = img[:, 0]
    right = img[:, -1]
 
    edges = np.concatenate([top, bottom, left, right])
    falses = reduce(lambda acc, edge_val: acc+1 if edge_val == False else acc, edges)
    trues = len(edges) - falses

    if trues > falses:
        return (img == False)       # switch boolean value of all pixels so background is "False"
    return img  #edges are already predominatly "False", so background should be as well

n = 20 
dim = (n,n)
train = glob.glob('train/*')
for img_path in train:
    img = imread(img_path) 
    img = resize(img, dim)
    img = rgb_to_binary(img)
    out_path = os.path.join('processed', os.path.basename(img_path))
    imsave(out_path, img)
