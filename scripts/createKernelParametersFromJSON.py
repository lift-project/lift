#!/usr/bin/python
import json
import sys
from pprint import pprint
from collections import OrderedDict

if len(sys.argv) <= 2: 
    print("Usage: python createKernelParametersFromJSON.py <json-file> <output-file>")
    sys.exit()

json_file = sys.argv[1] 
c_file = sys.argv[2] 

# json strings
sizeString = "sizes"
paramString = "parameters"
outputString = "outputs"
tmpBufString = "temporary buffers"

input_name = "inputParam"
output_name = "outputParam" 
tmp_buf_name = "tmpBuf"

kernel_name = "kernel"
cl_context = "context"
cl_queue= "queue"

args = 0

outfile = open(c_file,"w")

with open(json_file) as data_file:    
    data = json.load(data_file, object_pairs_hook=OrderedDict)
data_file.close()

# setup sizes first!
for i, (key,value) in enumerate(data[sizeString].items()):
    outfile.write("%s = ; // USER MUST FILL IN" % key)
    outfile.write("\n");

outfile.write("\n\n")

# INITIAL SETUP

# setup input parameters
for i, (key,value) in enumerate(data[paramString].items()):
    pType = key.split(" ")[0].strip()#.replace("*","") 
    if('*' in pType):
       outfile.write("%s %s%d = (%s)malloc(%s);" % (pType,input_name, i,pType,value))
    else:
       outfile.write("%s %s%d = ;// USER MUST FILL IN" % (pType, input_name, i))
    outfile.write("\n");
outfile.write("\n\n")

for i, (key,value) in enumerate(data[outputString].items()):
    pType = key.split(" ")[0].strip().replace("*","") 
    outfile.write("%s* %s%d = (%s*)malloc(%s);" % (pType,output_name, i,pType,value))
    outfile.write("\n");
outfile.write("\n\n")

for i, (key,value) in enumerate(data[tmpBufString].items()):
    pType = key.split(" ")[0].strip().replace("*","") 
    outfile.write("%s* %s%d = (%s*)malloc(%s);" % (pType,tmp_buf_name, i,pType,value))
    outfile.write("\n");
outfile.write("\n\n");

# OPENCL SETUP

# setup input opencl values
for i, (key,value) in enumerate(data[paramString].items()):
    #outfile.write(key.encode('ascii'), value.encode('ascii'))
    pType = key.split(" ")[0].strip()#.replace("*","") 
    pName = key.split(" ")[1].strip()
    if('*' in pType):
        outfile.write("cl_mem %s%d_d;" % (input_name, i))
        outfile.write("\n");
        outfile.write("%s%d_d = clCreateBuffer(%s, CL_MEM_READ_ONLY, %s, NULL, NULL);" %
                        (input_name, i, cl_context, value))
        outfile.write("\n");
        outfile.write("clEnqueueWriteBuffer(%s, %s%d_d, CL_TRUE, 0, %s, %s%d, 0, NULL, NULL);" %
                        (cl_queue,input_name, i, value, input_name,i ))
        outfile.write("\n");
        outfile.write("clSetKernelArg(%s, %d, sizeof(cl_mem), &%s%d_d);" 
                    % (kernel_name, args, input_name, i))
        outfile.write("\n");
    else:
        outfile.write("clSetKernelArg(%s, %d, sizeof(%s), &%s%d);" 
                % (kernel_name, args, pType, input_name,i ))
        outfile.write("\n\n");
    args+=1
outfile.write("\n\n");

# setup output opencl values
for i, (key,value) in enumerate(data[outputString].items()):
    outfile.write("cl_mem %s%d_d;" % (output_name, i))
    outfile.write("\n");
    outfile.write("%s%d_d = clCreateBuffer(%s, CL_MEM_WRITE_ONLY, %s, NULL, NULL);" %
                    (output_name, i, cl_context, value))
    outfile.write("\n");
    outfile.write("clEnqueueWriteBuffer(%s, %s%d_d, CL_TRUE, 0, %s, %s%d, 0, NULL, NULL);" %
                    (cl_queue,output_name, i, value, output_name,i ))
    outfile.write("\n");
    outfile.write("clSetKernelArg(%s, %d, sizeof(cl_mem), &%s%d_d);" 
                % (kernel_name, args, output_name, i))
    outfile.write("\n");
    args+=1
outfile.write("\n\n");

# setup temporary buffers opencl values
for i, (key,value) in enumerate(data[tmpBufString].items()):
    #outfile.write(key.encode('ascii'), value.encode('ascii'))
    outfile.write("cl_mem %s%d_d;" % (tmp_buf_name, i))
    outfile.write("\n");
    outfile.write("%s%d_d = clCreateBuffer(%s, CL_MEM_READ_WRITE, %s, NULL, NULL);" %
                    (tmp_buf_name, i, cl_context, value))
    outfile.write("\n");
    outfile.write("clEnqueueWriteBuffer(%s, %s%d_d, CL_TRUE, 0, %s, %s%d, 0, NULL, NULL);" %
                    (cl_queue,tmp_buf_name, i, value, tmp_buf_name,i ))
    outfile.write("\n");
    outfile.write("clSetKernelArg(%s, %d, sizeof(cl_mem), &%s%d_d);" 
                % (kernel_name, args, tmp_buf_name, i))
    outfile.write("\n");
    args+=1

# add sizes to kernel args
for i, (key,value) in enumerate(data[sizeString].items()):
    pType = key.split(" ")[0].strip().replace("*","") 
    pName = key.split(" ")[1].strip()
    outfile.write("clSetKernelArg(%s, %d, sizeof(%s), &%s);" 
                % (kernel_name, args, pType, pName))
    outfile.write("\n");
    args+=1
outfile.write("\n\n");

# CLEANUP
for i, (key,value) in enumerate(data[paramString].items()):
    pType = key.split(" ")[0].strip()#.replace("*","") 
    if('*' in pType):
        outfile.write("clReleaseMemObject(%s%d_d);" %(input_name, i))
        outfile.write("\n");
        outfile.write("free(%s%d);" % (input_name, i))
        outfile.write("\n");

for i, (key,value) in enumerate(data[outputString].items()):
    outfile.write("clReleaseMemObject(%s%d_d);" %(output_name, i))
    outfile.write("\n");
    outfile.write("free(%s%d);" % (output_name, i))
    outfile.write("\n");

for i, (key,value) in enumerate(data[tmpBufString].items()):
    outfile.write("clReleaseMemObject(%s%d_d);" %(tmp_buf_name, i))
    outfile.write("\n");
    outfile.write("free(%s%d);" % (tmp_buf_name, i))
    outfile.write("\n");
outfile.write("\n\n")


outfile.close()
