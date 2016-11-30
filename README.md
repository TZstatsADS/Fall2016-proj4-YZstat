# Project: Words 4 Music

### [Project Description](doc/Project4_desc.md)

![image](http://cdn.newsapi.com.au/image/v1/f7131c018870330120dbe4b73bb7695c?width=650)

Term: Fall 2016

+ [Data link](https://courseworks2.columbia.edu/courses/11849/files/folder/Project_Files?preview=763391)-(**courseworks login required**)
+ [Data description](doc/readme.html)
+ Contributor's name:
+ Projec title: Words and Music
+ Project summary: Use music data to estimate the distribution of the lyrics. 

For the music data:

After extract the h5data, I use Kmeans cluster to all of features contains in the h5 file excluded the "song". By the Kmeans cluster, I choose 20 as the levels of category. After extract the feature by this method I have 2350*15 matrix as the featrue.

Then about the lyrics, I use topic modelling, and I choose 20 topics. And choose the maximum probability as the only one topic.

About the association, I take supervised learning. By KNN method, I assigned each of the songs to every 20 topics, then combine the probality and the word rank of the topics. The final rank based on the summary rank of each kind of topic.


Sorry for the late update of the file direction.  

All related codes are in "doc" file. 

feature.R extract the music features both for train and test data.

test.R conduct the rank of the test data.

ads_proj4.R is full version of the project code. 
	
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
