# Pac-Man

In the following the browser game PacMan based on Elm is developed. Due to the latest browser policies in the browser Chrome the automatic playback of audio is no longer supported. We therefore recommend the use of Firefox or Edge. This document will be expanded in the course of development.

## How to start?

#### 1.  Initialize elm
First of all, the global Elm structure, which is individual in every development environment, must be initialized. To do this, the following command must be executed:

```elm
$ elm init
```

#### 2.  Compile the application
Before you start the application, you must compile the newest PacMan class as javascript file, as the following:

```elm
$ elm make src/PacMan.elm --output src/Assets/elm.js
```

#### 3.  Start the application
After this step, you can run the application by tiping the following command in terminal:
```elm
$ elm reactor
```

#### 4.  Open and Play
Now you can open the page by following this link [http://localhost:8000/src/index.html](http://localhost:8000/src/index.html)

## Dokumente
[Anforderungen](https://git.jt-networker.myds.me/tkramer/elm-pacman/wikis/Anforderungen)