%Step 3 of CBIG Kong2019 Brain Parc Pipeline
%
%To run: 1. Open Matlab using salloc (ex: `salloc --mem-per-cpu 6G --time 2:00:00 --x11`)
%	 2. source your config file containing the $CBIG_CODE_DIR variable
%	 3. `cd` to the $CBIG_CODE_DIR/stable_projects/brain_parcellation/Kong2019_MSHBM/step3... folder
% 	 4. `cp` this script over to the step3 folder in the CBIG repo
%	 5. Enter the command `ml matlab/r2018b`
%	 6. Enter the command `LD_PRELOAD= matlab`
%	 7. In Matlab: Pull up this script and choose "Run" (green button)
%	
%
%Previously, recon-all and the CBIG preproc pipeline were run on these subjects. 
%Additionally, you must have the folder structure and text files with paths to your preproc output set up 
%See the CBIG example script CBIG_MSHBM_create_example_input_data.sh for details on formatting.
%The script "Create_parc_data.sh" has taken care of this.
%Steps 1 and 2 have also been ran previous to this.
%
%
%For questions, contact M. Peterson, Nielsen Brain and Behavior Lab

%Note: Using the GSP Final_Params (FS6 space) from Ruby and CBIG. Per
%supplementary material for Kong2019 paper, the optimal parameters for the GSP dataset are c=30
%and alpha=200 (see page 10).


%% HCPD TASK AND REST
%%Part 1: Generate individual parcellation for each subject

%Subs with 1 run
sublist=[14 34 55 83 87 118 123 145 147 151 188 196 216 248 305 342 351 367 389 438 483 503 518 535];
project_dir = '/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/parc_output_fs6_HCPD/generate_individual_parcellations';
for subid = sublist
       CBIG_MSHBM_generate_individual_parcellation(project_dir,'fsaverage6','1','17',num2str(subid),'200','30');
end


%Subs with 2 runs
sublist=[8 13 52 73 90 102 110 135 191 224 232 253 260 283 292 300 328 341 348 357 364 376 385 394 409 436 453 471 496 499 614];
project_dir = '/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/parc_output_fs6_HCPD/generate_individual_parcellations';
for subid = sublist
       CBIG_MSHBM_generate_individual_parcellation(project_dir,'fsaverage6','2','17',num2str(subid),'200','30');
end


%Subs with 3 runs
sublist=[10 57 59 91 152 167 170 210 223 241 266 267 272 310 324 333 346 352 381 403 432 449 474 501 537 548 600 602 612 613]; 
project_dir = '/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/parc_output_fs6_HCPD/generate_individual_parcellations';
for subid = sublist
       CBIG_MSHBM_generate_individual_parcellation(project_dir,'fsaverage6','3','17',num2str(subid),'200','30');
end


%Subs with 4 runs
sublist=[24 36 45 48 50 60 63 65 96 127 153 212 269 287 316 338 388 413 476 502 513 514 527 544 579 609 610];
project_dir = '/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/parc_output_fs6_HCPD/generate_individual_parcellations';
for subid = sublist
       CBIG_MSHBM_generate_individual_parcellation(project_dir,'fsaverage6','4','17',num2str(subid),'200','30');
end



%Subs with 5 runs
sublist=[16 22 26 31 39 46 85 92 112 139 155 157 181 185 200 204 208 213 226 245 262 291 321 322 323 326 331 334 344 354 360 378 395 399 424 429 445 461 473 482 484 495 507 536 543 565 570 573 586]; 
project_dir = '/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/parc_output_fs6_HCPD/generate_individual_parcellations';
for subid = sublist
       CBIG_MSHBM_generate_individual_parcellation(project_dir,'fsaverage6','5','17',num2str(subid),'200','30');
end
  

%Subs with 6 runs
sublist=[1 18 43 44 56 64 68 78 81 82 100 101 106 124 126 154 193 206 209 215 220 242 243 246 257 258 293 295 308 315 327 353 370 382 396 401 417 421 439 447 457 470 472 477 480 493 508 534 541 547 552 561 591 593 608];
project_dir = '/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/parc_output_fs6_HCPD/generate_individual_parcellations';
for subid = sublist
       CBIG_MSHBM_generate_individual_parcellation(project_dir,'fsaverage6','6','17',num2str(subid),'200','30');
end


%Subs with 7 runs
sublist=[3 4 7 11 20 27 47 49 53 66 67 70 71 74 75 76 89 97 103 104 131 133 146 156 158 160 162 172 175 176 178 179 180 182 186 214 219 236 240 247 249 254 256 259 270 274 278 299 301 320 330 332 339 343 362 365 366 368 386 390 393 404 406 408 412 414 422 425 426 428 430 431 435 437 440 459 497 510 520 522 523 525 542 553 559 583 588 595 603 607]; 
project_dir = '/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/parc_output_fs6_HCPD/generate_individual_parcellations';
for subid = sublist
       CBIG_MSHBM_generate_individual_parcellation(project_dir,'fsaverage6','7','17',num2str(subid),'200','30');
end


%Subs with 8 runs
sublist=[2 5 9 15 17 19 21 23 28 32 33 35 37 41 51 54 58 61 69 77 80 88 93 94 95 99 105 108 109 116 120 121 122 125 130 132 136 137 150 161 165 166 168 169 173 174 183 187 192 194 195 198 202 203 205 217 218 221 222 225 228 234 235 239 250 251 252 255 261 263 264 279 281 284 286 289 290 296 297 303 307 311 318 319 336 337 340 345 355 356 358 369 371 373 374 375 377 380 387 391 398 405 407 410 423 427 434 441 442 444 448 450 452 455 456 460 465 468 481 488 490 491 494 498 500 505 506 509 511 515 516 517 526 528 530 531 532 533 538 540 549 550 551 554 555 556 557 558 562 563 564 567 568 569 572 576 577 580 582 584 587 589 599 601 604 611 615]; 
project_dir = '/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/parc_output_fs6_HCPD/generate_individual_parcellations';
for subid = sublist
       CBIG_MSHBM_generate_individual_parcellation(project_dir,'fsaverage6','8','17',num2str(subid),'200','30');
end


%Subs with 9 runs
sublist=[6 12 25 29 30 38 40 42 62 72 79 84 86 98 107 111 113 114 115 117 119 128 129 134 138 140 141 142 143 144 148 149 159 163 164 171 177 184 189 190 197 199 201 207 211 227 229 230 231 233 237 238 244 265 268 271 273 275 276 277 280 282 285 288 294 298 302 304 306 309 312 313 314 317 325 329 335 347 349 350 359 361 363 372 379 383 384 392 397 400 402 411 415 416 418 419 420 433 443 446 451 454 458 462 463 464 466 467 469 475 478 479 485 486 487 489 492 504 512 519 521 524 529 539 545 546 560 566 571 574 575 578 581 585 590 592 594 596 597 598 605 606 616]; 
project_dir = '/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/parc_output_fs6_HCPD/generate_individual_parcellations';
for subid = sublist
       CBIG_MSHBM_generate_individual_parcellation(project_dir,'fsaverage6','9','17',num2str(subid),'200','30');
end


%% HCPD REST ONLY

%No subs with just one run due to splitting (see step1)

%%Subs with 2 runs
%Load text file
    filename = '/nobackup/scratch/grp/fslg_spec_networks/code/HCPD_analysis/Kong2019_parc_fs6/sess_numbers/2_sess.txt';
    delimiter = {''};
    % Format for each line of text:
    %   column1: text (%s)
    % For more information, see the TEXTSCAN documentation.
    formatSpec = '%s%[^\n\r]';
    % Open the text file.
    fileID = fopen(filename,'r');
    % Read columns of data according to the format.
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string',  'ReturnOnError', false);
    % Close the text file.
    fclose(fileID);
    sublist = [dataArray{1:end-1}];
    clearvars filename delimiter formatSpec fileID dataArray ans;

project_dir = '/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/parc_output_fs6_HCPD_REST/generate_individual_parcellations';
for subid = 1:length(sublist)
        subject=sublist(subid);
       CBIG_MSHBM_generate_individual_parcellation(project_dir,'fsaverage6','2','17',subject,'200','30');
end


%%Subs with 3 runs
%Load text file
    filename = '/nobackup/scratch/grp/fslg_spec_networks/code/HCPD_analysis/Kong2019_parc_fs6/sess_numbers/3_sess.txt';
    delimiter = {''};
    % Format for each line of text:
    %   column1: text (%s)
    % For more information, see the TEXTSCAN documentation.
    formatSpec = '%s%[^\n\r]';
    % Open the text file.
    fileID = fopen(filename,'r');
    % Read columns of data according to the format.
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string',  'ReturnOnError', false);
    % Close the text file.
    fclose(fileID);
    sublist = [dataArray{1:end-1}];
    clearvars filename delimiter formatSpec fileID dataArray ans;

project_dir = '/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/parc_output_fs6_HCPD_REST/generate_individual_parcellations';
for subid = 1:length(sublist)
        subject=sublist(subid);
       CBIG_MSHBM_generate_individual_parcellation(project_dir,'fsaverage6','3','17',subject,'200','30');
end


%%Subs with 4 runs
%Load text file
    filename = '/nobackup/scratch/grp/fslg_spec_networks/code/HCPD_analysis/Kong2019_parc_fs6/sess_numbers/4_sess.txt';
    delimiter = {''};
    % Format for each line of text:
    %   column1: text (%s)
    % For more information, see the TEXTSCAN documentation.
    formatSpec = '%s%[^\n\r]';
    % Open the text file.
    fileID = fopen(filename,'r');
    % Read columns of data according to the format.
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string',  'ReturnOnError', false);
    % Close the text file.
    fclose(fileID);
    sublist = [dataArray{1:end-1}];
    clearvars filename delimiter formatSpec fileID dataArray ans;

project_dir = '/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/parc_output_fs6_HCPD_REST/generate_individual_parcellations';
for subid = 1:length(sublist)
        subject=sublist(subid);
       CBIG_MSHBM_generate_individual_parcellation(project_dir,'fsaverage6','4','17',subject,'200','30');
end


%%Subs with 5 runs
%Load text file
    filename = '/nobackup/scratch/grp/fslg_spec_networks/code/HCPD_analysis/Kong2019_parc_fs6/sess_numbers/5_sess.txt';
    delimiter = {''};
    % Format for each line of text:
    %   column1: text (%s)
    % For more information, see the TEXTSCAN documentation.
    formatSpec = '%s%[^\n\r]';
    % Open the text file.
    fileID = fopen(filename,'r');
    % Read columns of data according to the format.
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string',  'ReturnOnError', false);
    % Close the text file.
    fclose(fileID);
    sublist = [dataArray{1:end-1}];
    clearvars filename delimiter formatSpec fileID dataArray ans;

project_dir = '/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/parc_output_fs6_HCPD_REST/generate_individual_parcellations';
for subid = 1:length(sublist)
        subject=sublist(subid);
       CBIG_MSHBM_generate_individual_parcellation(project_dir,'fsaverage6','5','17',subject,'200','30');
end
