#!/usr/bin/perl

# Make a video from the oW5 classification images

use strict;
use warnings;

# Put the selected images in a temporary directory
my $Tdir = "/var/tmp/hadpb/$$";
mkdir($Tdir) or die "Couldn't make $Tdir";

my $Count = 0;
my $Glob  = "/data/local/hadpb/images/oW5.working/*.png";
foreach my $ImageFile ( glob($Glob) ) {
    unless ( -r $ImageFile ) { die "Missing image $ImageFile"; }
     my $Nfname = sprintf "%s/%04d.png", $Tdir, $Count++;
    `cp  $ImageFile $Nfname`;
}

`ffmpeg -qscale 3 -r 12 -i $Tdir/%04d.png cls.video.mp4`;

`rm -r $Tdir`;
