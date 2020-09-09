# Pac-Man

In the following the browser game PacMan based on Elm is developed. Due to the latest browser policies in the browser Chrome the automatic playback of audio is no longer supported. We therefore recommend the use of Firefox or Edge. This document will be expanded in the course of development.

## 💡 How to start?

### 1.  Compile the application
Before you start the application, you must compile the newest PacMan class as javascript file, as the following:

```elm
$ elm make src/Main.elm --optimize --output src/Assets/elm.js
```

### 2.  Start the application
After this step, you can run the application by tiping the following command in terminal:
```elm
$ elm reactor
```

### 3.  Open and play
Now you can open the page by following this link [http://localhost:8000/src/index.html](http://localhost:8000/src/index.html). 

## 🎯 Alternative run application
You can also easily view and test the application on the <a href="https://timokramer4.github.io/elm-pacman" target="_blank">GitHub Page</a> without having to compile it yourself in your development environment.



## 🔨 Instruction manual
The game starts directly when you enter the page.

### 1.  Key commands
To move PacMan the arrow keys of the keyboard are used. 
To pause or resume the game the escape key is used.


## 📚 Documents
See [Requirements](https://git.jt-networker.myds.me/tkramer/elm-pacman/wikis/Anforderungen)