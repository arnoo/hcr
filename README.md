Keep is a free (GPL licensed) tool for the safe-keeping of important files against degradation through the slow accumulation of hardware and software errors ("data rot").

/!\ This is a work in progress and not indented for public consumption yet !


Warning: Keep is alpha software, entrust your precious files to it at your own risk.
Keep is not a replacement for regular backups.

Keep
  * creates a digest file (with a kmd - Keep MetaData - extension) that it stores next to your files.
  * can synchronize multiple copies of your files, ensuring errors are not replicated
  * repairs broken files from a copy that is either sound or broken in another place (Keep checksums your files by chunks for this purpose)
  
Example use cases:
  * You keep your family pictures on your personal NAS. Keep computes checksums for all your photos, and synchronizes additions to a backup server (or an online service). When a picture on either the master or the backup servers is broken, Keep logs the issue and repairs the broken file.
  * You receive your *income statement* by email. Attached along with the statement is a kmd file. You keep both in your email, and a copy in your personal computer. Keep can be used to check the validity of either file and repair them if necessary.
  

Command line interface

Compute metadata
    $ keep hash <file> [<file>]*
    $ keep hash family_pictures/**.jpg # this creates one kmd file for each of your pictures
    
Check the integrity of your files
    $ keep check <file> [<file>]*
    $ keep check family_pictures
    
Backup your files other sshfs
    $ mount -t sshfs /mnt/backup_server
    $ keep sync --repair-src family_pictures /mnt/backup_server/data  # put this in your crontab to keep the copy up to date. Files on the copy are automatically repaired from the original if broken. --repair-src means that 



Building Keep from source

Install ECL (*TODO*)
clone the Keep Git repository
make