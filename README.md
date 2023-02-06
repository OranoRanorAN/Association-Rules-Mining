# association-rules-mining
HW4 for Data Mining

## 数据集描述
order_products_train.csv为交易表；product.csv为商品名和商品编号的对应关系

### 数据分析
1. 针对交易表挖掘关联规则: 哪个商品最常和哪个商品同时被购买?
2. 使用两种算法(Apriori 算法、FP-growth 算法)分别进行数据挖掘，并汇报 2 种算法的效率

<p float="left" align="left">
  <img src="/关联规则挖掘/算法比较.png" / width="500">
</p>

3. 设置不同的算法参数(置信度，支持度)，看结果有何不同

<p float="left" align="left">
  <img src="/关联规则挖掘/apr with different support levels.jpg" / width="600">
</p>

4. 规则可视化

<p float="left" align="left">
  <img src="/关联规则挖掘/可视化/graph.png" / width="500">
</p>
