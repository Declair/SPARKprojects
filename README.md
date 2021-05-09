# SPARKprojects
 Ada and SPARK for High Integrity subject `SWEN90010 High Integrity Systems Engineering`
 
 ## Note
 
 Based on the regulations of GPS, an Ada project should have a folder for source files and another folder for object files (compiled caches).
 
There are a lot of temporary files in the **obj** folder, so I ignored all the files in this folder in the .gitignore. However, by this way, there will be no **obj** folder in the git repository. If pulling the project somewhere else, GPS will report an error that no **obj** folder found.

To solve this, I manually add an **aaa.txt** file in **obj** folder for every Ada project, and ask git not to ignore that file.
