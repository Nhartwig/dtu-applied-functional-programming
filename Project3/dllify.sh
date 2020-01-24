fsharpc -a GameLogic/GameLogic.fsi GameLogic/GameLogic.fs
fsharpc -a AI/AI.fsi AI/AI.fs
fsharpc -a GUI/GUI.fsi GUI/GUI.fs -o:GUI.dll
fsharpc -a GUI/GUI.fsi GUI/BasicGUI.fs -o:BasicGUI.dll
