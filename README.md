# imageCompressor
```
$ make
```
```
$ ./imageCompressor -h
USAGE: ./imageCompressor -n N -l L -f/-p F/P
        N       number of colors in the final image
        L       convergence limit
        F       path to the file containing the colors of the pixels
        P       path to the file .png
```

```
$ cat p2.txt 
(0,0) (66,20,26)
(0,1) (98,99,233)
(0,2) (45,12,167)
(1,0) (33,16,94)
(1,1) (78,8,9)
(1,2) (20,27,67)
(2,0) (88,77,211)
(2,1) (1,56,37)
(2,2) (15,89,40)

$ ./imageCompressor -f p2.txt -l 0.8 -n 2
--
(36,36,46)
-
(0,0) (66,20,26)
(1,0) (33,16,94)
(1,1) (78,8,9)
(1,2) (20,27,67)
(2,1) (1,56,37)
(2,2) (15,89,40)
--
(77,63,204)
-
(0,1) (98,99,233)
(0,2) (45,12,167)

(2,0) (88,77,211)
```
```$ ./imageCompressor -p ressources/sky.png -l 0.9 -n 4```

![smale2](https://user-images.githubusercontent.com/72013068/166877084-888ef93d-1964-48b8-a290-e41e8ede6ed0.png) ->
![newclust](https://user-images.githubusercontent.com/72013068/166877071-5fe9c992-7a91-4836-987a-6a776f6b47ee.png)

```$ ./imageCompressor -p ressources/ananas.png -l 0 -n 4```

![smale](https://user-images.githubusercontent.com/72013068/166878832-9aae62a1-94e2-4135-9391-400fc4cc79e3.png) ->
![newclust](https://user-images.githubusercontent.com/72013068/166879236-be96b322-9395-4252-871b-87c10def111e.png)
