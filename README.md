Hcr
===

Hcr (Hash, Check, Repair) is a free (GPL licensed) tool for checking file integrity and repairing broken files.

Files can degrade through the slow accumulation of hardware and software errors ("data rot").
Hcr creates a digest file that you can store, send and back up along with each of your important files, using your usual tools.
You can use the digest file to check the validity of copies periodically and avoid propagating errors accross backups.
Hcr checksums your file by chunks and can use the checksums to produce a valid file from two copies broken in different places.

![Hcr chunk hashing diagram](https://cloud.githubusercontent.com/assets/185428/10698713/bed8b41e-79b2-11e5-9534-479fa484eada.png)

/!\ This is a work in progress and not indented for public consumption yet !

Warning: Hcr is alpha software, entrust your precious files to it at your own risk.
Hcr is not a replacement for regular backups.


Command line interface
======================

Compute (or update) metadata

    $ hcr hash <file> [<file>]*
    $ hcr hash family_pictures/**.jpg # this creates one kmd file for each of your pictures
    
Check the integrity of your files

    $ hcr check <file> [<file>]*
    $ hcr check family_pictures # Will check all hashed files in folder family_pictures and its subfolders
    
Repair a file based on a copy

    $ hcr repair <file> <copy>*


Building Hcr from source
=========================

Install ECL (There are packages for most Linux distributions, and you can refer to the [manual](https://common-lisp.net/project/ecl/manual/pr01s06.html) to install from source)
clone the Hcr Git repository
make
