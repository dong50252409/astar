astar
=====

高性能A星算法实现，使用pairs_heap优化Open列表，去掉原有Closed列表改为Visited记录已经开启坐标，使用曼哈顿距离作为启发函数。

支持 4、6（六边形）、8 方向寻路，支持限制最大搜索深度

构建 Build
-----

    $ rebar3 compile

单元测试 Eunit
----

    $ rebar3 eunit

如何使用 How to use
-----

    1> StartGrid = {1, 1}.
    2> EndGrid = {50, 50}.
    2> BlockList = [{47,1},{24,2}, {2,25}, {20,31}, {20,21}, {50,20}, ...].
    3> ValidFun = fun({X,Y}) -> not lists:member({X,Y}, BlockList) end.
    4> Options = [],
    5> {max, Path} = astar:search(StartGrid, EndGrid, ValidFun, Options).
