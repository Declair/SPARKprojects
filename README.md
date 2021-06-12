# SPARKprojects
 Ada and SPARK for High Integrity subject `SWEN90010 High Integrity Systems Engineering`
 
 ## Note
 
 Based on the regulations of GPS 2019, an Ada project should have a folder for source files and another folder for object files (compiled caches).
 
There are a lot of temporary files in the **obj** folder, so I ignored all the files in this folder in the .gitignore. However, by this way, there will be no **obj** folder in the git repository. If pulling the project somewhere else, GPS will report an error that no **obj** folder found.

To solve this, I manually add an **aaa.txt** file in **obj** folder for every Ada project, and ask git not to ignore that file.

## Contents

All specification docs are in /\_docs.

- first spark
    - Hello world, simple stack with SPARK annotation
- spark loop
    - Very simple loop with loop invariant
- workshop 5
    - String
    - Simple wordcount program
- workshop 6
    - Ada packages
    - Ada records
- workshop 7
    - Basic SPARK properties
- workshop 8
    - Division program, loop invariant
- workshop 10
    - SPARK pointer
- workshop 11
    - Voting algorithms implemented in Ada
- assignment 3
    - Small command-line Desktop Calculator utility
