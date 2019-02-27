# scala实现遗传算法解决0-1背包问题
## 测试数据
根目录下data文件夹下数据data1-data10。result.txt是每个测试数据的正确结果。
## 程序运行
安装sbt。官网:https://www.scala-sbt.org/  
安装好sbt后，进入程序根目录,打开命令行输入 `sbt` 进入sbt交互界面，第一次运行sbt的话需要一段时间来下载sbt依赖包。  
进入sbt交互界面后输入 `run` 开始运行程序。  
![Image text](https://github.com/Yves-yuan/backpack-scala/raw/master/document/img/run.PNG)  
程序在规定时间内会迭代计算多次，得到一个最优解。在日志输出中看到的maxFit:2614表示本次迭代后得到的最优解为2614,weight:999
表示最优解中的背包重量为999。在data文件夹中的result.txt中可以确认程序计算结果是否正确。  
程序默认设置运行时间为4秒钟，但是在测试数据中，要计算出正确答案一般只需要1~2秒。如果需要处理更大的测试数据集，可以适当调整
运行时间参数，代码中的`maxTime`参数就是程序运行的最大时间。  
欢迎补充更多的测试数据，欢迎hack!  
## 遗传算法
https://baike.baidu.com/item/%E9%81%97%E4%BC%A0%E7%AE%97%E6%B3%95/838140?fr=aladdin  
是一种随机搜索算法，通过模拟生物进化来优化搜索方向。
## 作者邮箱
yuanp0813@163.com