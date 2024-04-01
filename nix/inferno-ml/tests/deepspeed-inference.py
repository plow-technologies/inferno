from transformers import pipeline
import transformers
import deepspeed
import torch
import os
from transformers.models.bert.modeling_bert import BertLayer

pipe = pipeline('fill-mask', model='bert-large-cased', device=-1)
pipe.model = deepspeed.init_inference(pipe.model, mp_size=0, dtype=torch.float)
device = 'cuda:0' if torch.torch.cuda.is_available() else 'cpu'
pipe.device = torch.device(device)
output = pipe("In Autumn the [MASK] fall from the trees.")
print(output)
