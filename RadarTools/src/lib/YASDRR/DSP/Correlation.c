/**
Copyright 2017 Robert Christian Taylor

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

/**
 * Performs complex correlation on a signal. Should be autovectorized by the C
 * compilier. For best performance, compile with --mach=native.
 * @param kSize The size of the correlation kernel
 * @param sSize The size of the signal
 * @param kernel Pointer to the array of kernel data
 * @param signal Pointer to the array of signal data
 * @param output Pointer to the array to write the output to. Should equal
 * the signal array in size.
 */
__attribute__((hot))
void complexCorrelate(int kSize, int sSize, double * kernel, double * signal, double * output) 
{
    int i;
    int j;
    double Raccumulator;
    double Iaccumulator;
    for(i = 0; i < sSize; i++) 
    {
        Raccumulator = 0;
        Iaccumulator = 0;
        for(j = 0; j < kSize && (j+i) < sSize; j++)
        {
            double a = kernel[2*j];
            double b = kernel[2*j + 1];
            
            double c = signal[2*i + 2*j];
            double d = signal[2*i + 2*j + 1];
            
            //Signs are flipped because the kernel is conjugated.
            Raccumulator += a * c  + b * d;
            Iaccumulator += a * d - b * c;
        }
        
        output[2*i] = Raccumulator;
        output[2*i + 1] = Iaccumulator;
    }
} 
