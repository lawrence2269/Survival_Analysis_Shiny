# Survival analysis via Shiny app
An application develped in RShiny for performing survival analysis. My lab colleagues are currently involved in a cancer research which requires to perform survival analysis between mutations in patients. They all are biologists and don't have knowledge about coding. Hence, it is difficult for them to perform this task. They either need to use IBM SPSS, but it requires license to operate or need to outsource this task for a cost, but we lose confidentiality. In order to tackle this bottleneck efficiently, I came up with a proposal to build a tool that can be <strong>customized</strong>, <strong>cost-effective</strong> and <strong>open source</strong>. Currently, only basic functionalities have been implemented. Advanced functionalities can be included on request.
<br>
<h3> <u>Technical stack </u></h3>
<ul> 
  <li> R </li>
  <li> <h4> Libraries </h4> </li>
    <ul>
      <li> shiny </li>
      <li> shinythemes </li>
      <li> shinyvalidate </li>
      <li> shinyjs </li>
      <li> shinyalert </li>
      <li> dplyr </li>
      <li> survminer </li>
      <li> survival </li>
      <li> ggplot2 </li>
    </ul>
</ul>
<br>
<h3> Inputs </h3>
<ul>
  <li> <strong> File format: </strong> only .csv files are allowed </li>
  <li> <h3> File content formats </h3> </li>
  <ul>
    <li> <strong> Time </strong> has to be in days or weeks or months and not in DD-MM-YYYY, MM-DD-YYYY, and etc. </li>
    <li> <strong> Status </strong> has to be a integer variable with 0's and 1's. </li>
    <li> <strong> Factor </strong> has to be a categorical variable. </li>
    <ul>
      <li> <strong> With </strong> contains values from <strong> Factor </strong> </li>
      <li> <strong> Against </strong> contains values from <strong> Factor </strong> </li>
    </ul>
  </ul>
</ul>
<br>
<h3> Input screenshots </h3>
<img src="/assets/Input_1.JPG" width="50%">
<img src="/assets/Input_2.JPG" width="50%">
<img src="/assets/Input_3.JPG" width="50%">
