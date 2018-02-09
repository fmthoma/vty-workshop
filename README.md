Workshop: Terminal GUIs with Haskell
================================================================================

Prerequisites:
--------------------------------------------------------------------------------

* Linux/Unix machine or VM: Unfortunately, Vty does not support Windows
  currently. You can try with Win10 and WSL, but I'd rather recommend a Linux VM
  if you're on Windows.

* You'll need a working [Stack](haskellstack.org) installation to build
  the project.

* Clone or download this repository and check that the build works:

        git clone https://github.com/fmthoma/vty-workshop
        cd vty-workshop/code
        stack build
        stack exec vty-test

    You should see the following text in your terminal:
    
        It works!
        Press any key to continue
