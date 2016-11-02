# Project: Labradoodle or Fried Chicken? 
![image](https://s-media-cache-ak0.pinimg.com/236x/6b/01/3c/6b013cd759c69d17ffd1b67b3c1fbbbf.jpg)
### [Full Project Description](doc/project3_desc.html)

Term: Fall 2016

+ Team 8
+ Team members
	+ Yiwei Sun 
	+ Xuechun Sun
	+ Jiani Tian
	+ Shujin Cao
	+ Ying Zhu
+ Project summary: In this project, we created a classification engine for images of poodles versus images of fried chickens. 
	
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

Yiwei Sun: 
- Train random forest model based on SIFT
- Extract the rgb feature; Test the advance model

Jiani Tian: Train Neural Network model based on SIFT; Try the face detection in dog; Test the advance model

Xuechun Sun: Train GBM model based on SIFT; Test the baseline model and advance model; Apply Median Filter and build Codebook using ORB methods and PCA to reduce the dimension;

Shujin Cao: Train SVM model based on SIFT; Test the baseline model and advance model;

Ying Zhu: Train SVM model based on SIFT; Determine the baseline model by tuning parameter; Test the baseline model and advance model;



Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
