#!/usr/bin/env sh
# Compute the mean image from the imagenet training lmdb
# N.B. this is available in data/ilsvrc12

EXAMPLE=__CAFFEDIR__/examples/__NAME__
DATA=__CAFFEDIR__/data/__NAME__
TOOLS=__CAFFEDIR__/build/tools

$TOOLS/compute_image_mean -backend=lmdb $EXAMPLE/__NAME___train_lmdb \
  $DATA/__NAME___train_mean.binaryproto

$TOOLS/compute_image_mean -backend=lmdb $EXAMPLE/__NAME___val_lmdb \
  $DATA/__NAME___val_mean.binaryproto

echo "Done."
