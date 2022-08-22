# BSSG 

## Package containing functions used at BSSG

## Function list:

**Functions for dataimport**

These functions are used to load .txt files from the software dataquest ART, named "Variable_Group_ID", where ID is numeric

- `list_files()` - creates a list of file paths for each animal
- `load_single_file()` - loads a *single* .txt file from a single animal
- `combine_files()` - create a data set from several .txt files from *a single animal*
- `create_data()` - create a data set froma list of several .txt files from *several animals*

**Functions for data formatting**

These functions are used for formatting a dataset created by the functions above

- `format_normalize()` - normalises a variable towards an experimental period
    


test
