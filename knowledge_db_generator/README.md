This collection of tools is used to interract with the board knowledge database.
So far the size of the database is about 424Kbytes for all the packs (without the interrupt vector)

- 2 python tools:
    - modifybdb: handles the operations that modify the database
        + constructing the tables
        + adding a package

    - querybdb: handles all the query operations:
        + query all devices.
        + query all packages.
        + query the info regarding a specific device (by name).

