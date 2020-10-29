#CHANGELOG

## Async 0.0.3

New routines to save and load async objects as `rds` files ;
New progress bar with cancel button;
 

### New Methods/Functions/Classes 

- `asincBar2[R6]` : Reports  async class progress with a custom shiny bar to end user;
- `async$inc` :Increment the progress tracking;
- `load_async` : Functions to read a single async object to a  RDS file;
- `save_async` :  Functions to write a single async object to a  RDS file.
- `asincBar2$close` : display  a cancel button to end user associated with the process tracked by the async class

### Changes

- Async param detail added ;
- `asyncBar` name changed to `asyncBar1` ;
- The package documentation has been rewritten.

### Bugs
- `asyn: get_status` method  character(0) error fixed;



