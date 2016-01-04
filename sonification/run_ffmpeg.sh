ffmpeg -r 24 -pattern_type glob -i $SCRATCH/images/oW5.working/\*.png -i sound.mp4 -c:v libx264 -preset slow -tune animation -profile:v high -level 4.2 -pix_fmt yuv420p -crf 22  sound.video.mp4

