# expressnim
Implementing ISO 10303-11, 21, and 22 in Nim.
  + Part 11 - The EXPRESS data modeling language.
    * Used to describe data structures, functions, and procedures in a universal format.
    * Mostly used to describe schema used in STEP (.stp) files.
  + Part 21 - Clear text encoding for product data exchange
    * Facilitates product data transfer and distribution between multiple computer systems.
    * Better known as a STEP file.
  + Part 22 - Standard data access interface (api) for STEP files.
    * Allows for interaction with the data contained in stp files.
 
The project is still under development.
The goal is to create a full implementation of ISO 10303-11, 21, and 22 in Nim.
That is: parse EXPRESS files and thereby STEP files, and provide an SDAI api for
ease of use.
There really aren't that many of these types of programs around! Especially
open-source ones.
The hope is that this will lead to some much need innovation in the CAD/CAM
industry.

### TODO/GOALS:
  + Create a standard environment for handling STEP files.
  + Create a functional environment for handling STEP files.
