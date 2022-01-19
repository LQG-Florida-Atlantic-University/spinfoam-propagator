# Spinfoam propagator

#### 介绍
The mathematica code for calculating the Lorentzian spin foam propagator

#### 使用说明

1.  基于wolframscript
2.  使用方法： wolframscript ActionDerivative-64xgAMMCEB.wls [$\lambda$] [Working Path] [Number of Kernels] [FlowTime] [Number of Burning in Cycles] [Method] [Regulator]
3.  [Method]=0 加速ODE算法，[Method]=1 Tanizaki 正规化加速ODE， [Method]=2 Mathematica 自带ODE 算法。使用Tanizaki正规化时需设定regulator
4.  边界文件 savedBoundary.wl 需置于 WorkingPath 中
5.  email 文件 email.wl 需置于 WorkingPath 中
6.  需预先在文件中设定发件邮件地址，密码，stmp服务器以及收件邮件地址

#### 参与贡献

1.  Fork 本仓库
2.  新建 Feat_xxx 分支
3.  提交代码
4.  新建 Pull Request


#### 特技

1.  使用 Readme\_XXX.md 来支持不同的语言，例如 Readme\_en.md, Readme\_zh.md
2.  Gitee 官方博客 [blog.gitee.com](https://blog.gitee.com)
3.  你可以 [https://gitee.com/explore](https://gitee.com/explore) 这个地址来了解 Gitee 上的优秀开源项目
4.  [GVP](https://gitee.com/gvp) 全称是 Gitee 最有价值开源项目，是综合评定出的优秀开源项目
5.  Gitee 官方提供的使用手册 [https://gitee.com/help](https://gitee.com/help)
6.  Gitee 封面人物是一档用来展示 Gitee 会员风采的栏目 [https://gitee.com/gitee-stars/](https://gitee.com/gitee-stars/)
