#!/usr/bin/python

import os
import pandas
import numpy as np
from shutil import copyfile
from skimage.io import imread

df = pandas.read_csv('trainLabels.csv')
A = df[df['Class'] == 'A']

for ID in A['ID']:
    print ID
    d = os.path.join('As', str(ID))
    os.mkdir(d)

    img_name = '{0}.png'.format(ID)
    processed = os.path.join('processed', img_name)
    original = os.path.join('train', img_name)

    copyfile(processed, os.path.join(d, 'processed.png'))
    copyfile(original, os.path.join(d, 'original.png'))

    bw = imread(processed)
    np.savetxt(os.path.join(d, 'matrix'), bw, fmt='%d')

