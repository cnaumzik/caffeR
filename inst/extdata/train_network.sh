#!/usr/bin/env sh
TOOLS=__CAFFEDIR__/build/tools/caffe
MODEL=__CAFFEDIR__/models
sudo $TOOLS train \
--solver $MODEL/__NAME__/solver.prototxt \
--weights $MODEL/__NETWORK__/__NETWORK__.caffemodel 2>&1 \
| tee $MODEL/__NAME__/model.log
