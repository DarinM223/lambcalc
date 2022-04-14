llc main.ll --relocation-model=pic && gcc main.s -o main && ./main; echo $?
