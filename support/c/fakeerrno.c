/*
 * The content of this file is in the public domain.
 *
 * Rather than patching out all use of errno in GHC, this provides a fake
 * errno system.  It's obviously completely bogus, but that's OK for our use.
 */

int errno = 0;

static const char* nothing = "";

const char* strerror()
{
    return nothing; 
}

int* __errno_location()
{
    return &errno;
}

