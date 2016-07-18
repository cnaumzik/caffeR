#!/usr/bin/env sh
# Create the lmdb inputs
# N.B. set the path to the train + val data dirs

EXAMPLE=__CAFFEDIR__/examples/__NAME__
DATA=__CAFFEDIR__/data/__NAME__
TOOLS=__CAFFEDIR__/build/tools

TRAIN_DATA_ROOT=__CAFFEDIR__/data/__NAME__/train/
VAL_DATA_ROOT=__CAFFEDIR__/data/__NAME__/val/

# Set RESIZE=true to resize the images to 256x256. Leave as false if images have
# already been resized using another tool.
RESIZE=__RESIZE_FLAG__
if $RESIZE; then
  RESIZE_HEIGHT=__RESIZE_HEIGHT__
  RESIZE_WIDTH=__RESIZE_WIDTH__
else
  RESIZE_HEIGHT=0
  RESIZE_WIDTH=0
fi

if [ ! -d "$TRAIN_DATA_ROOT" ]; then
  echo "Error: TRAIN_DATA_ROOT is not a path to a directory: $TRAIN_DATA_ROOT"
  echo "Set the TRAIN_DATA_ROOT variable in create_imagenet.sh to the path" \
       "where the ImageNet training data is stored."
  exit 1
fi

if [ ! -d "$VAL_DATA_ROOT" ]; then
  echo "Error: VAL_DATA_ROOT is not a path to a directory: $VAL_DATA_ROOT"
  echo "Set the VAL_DATA_ROOT variable in create_imagenet.sh to the path" \
       "where the ImageNet validation data is stored."
  exit 1
fi

echo "Creating train lmdb..."

GLOG_logtostderr=1 $TOOLS/convert_imageset \
    --resize_height=$RESIZE_HEIGHT \
    --resize_width=$RESIZE_WIDTH \
    --shuffle \
    $TRAIN_DATA_ROOT \
    $DATA/train.txt \
    $EXAMPLE/__NAME___train_lmdb

echo "Creating val lmdb..."

GLOG_logtostderr=1 $TOOLS/convert_imageset \
    --resize_height=$RESIZE_HEIGHT \
    --resize_width=$RESIZE_WIDTH \
    --shuffle \
    $VAL_DATA_ROOT \
    $DATA/val.txt \
    $EXAMPLE/__NAME___val_lmdb

echo "Done."
