# vrom_compiler<br>
##### vrom compiler<br>
##### need ecl <br>
##### code data format : txt , addr first ,data next; addr and data in one line<br>
##### :example :<br>
##### root # cat shit320x9.cod<br>
##### 000000000 101101000<br>
##### 000000001 101000101<br>
##### 000000010 100101010<br>
##### .....<br>
### screen shot (view in klayout)  <br>
### ![Image](https://github.com/BHa2R00/vrom_compiler/blob/master/2018-04-30%2021-29-19screenshot.png)
### screen shot (view in calibredrv) <br>
### ![Image](https://github.com/BHa2R00/vrom_compiler/blob/master/2018-04-30%2022-10-56screenshot.png)
### How to use
#### first, you need 'leaf cell' and the corresponding PDK
#### second, you need 'ecl' and 'make'
#### then,edit 'vrom_compile.lisp' and 'config.lisp'
#### make
#### ./vrom_compile
