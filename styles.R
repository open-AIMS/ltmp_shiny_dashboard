
tag_styles <- tags$style(HTML(
  '
    .skin-blue .main-header .logo {
      background-color: #00486A;
      color: #fff;
      border-bottom: 0;
      text-align: left;
      padding: 0px;
    }

    .skin-blue .main-header .logo:hover {
      background-color: #00486A;
      color: #fff;
    }

    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%;                /* Set width for container */
      max-width: 400px;
    }

    .label-left .control-label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 100px;          /* Target width for label */
    }

    .label-left .form-control {
      flex-basis: 300px;          /* Target width for slider */
    }

    .fa-circle-check {
      color: green; 
    }
        
    .fa-clock {
      color: orange;
    }

    .fa-circle-exclamation {
      color: orange;
    }

    .fa-circle-xmark {
      color: red;
    }

    #log_output {
      height: 200px;
      overflow-y: auto;
      display:flex;
      flex-direction: column-reverse;
    }
    #model_log_output {
      height: 200px;
      overflow-y: auto;
      display:flex;
      flex-direction: column-reverse;
    }

    .callout {
  border-left: 3px solid #BBB;
  background-color: #EEE;
  display: block;
  position: relative;
  overflow: auto;
  }

  .call-info {
  border: 1px solid #0288D1;
  background-color: #D3EFFF;
  font-size: 17px;
  border-left: 3px solid #0288D1;
}

.callout h4 {
  color: black;
font-weight: 500;
}
.call-info h4::before {
  content: "🛈" ;
  color: #0288D1;
  padding-right: 10px;
  vertical-align: middle;
  display: inline-block;

}

.external_out pre {
  padding: 0px;
  font-size: 10pt;
  background-color: white;
  border: none;
}

details {
margin-top: -10px;
}
details[open] {
  border: 1px solid #0288D1;
  background-color: #D3EFFF;
  border-left: 3px solid #0288D1;
  padding-left:5px;

}

ul {
padding-bottom:10px;
}

  //.details-info {
  //border: 1px solid #0288D1;
  //background-color: #D3EFFF;
  //border-left: 3px solid #0288D1;
  //padding-left:5px;
//}

summary {
 display:list-item;
}

.table-minimal table {
  border: 2px solid #000000;
  width: 100%;
  text-align: left;
  border-collapse: collapse;
}
.table-minimal td, .table-minimal th {
  border: 1px solid #000000;
  padding: 5px 4px;
}
.table-minimal tbody td {
  font-size: 13px;
}
.table-minimal thead {
  background: #D0E4F5;
  border-bottom: 1px solid #000000;
}
.table-minimal thead th {
  font-size: 15px;
  font-weight: bold;
  color: #000000;
  text-align: left;
  border-left: 1px solid #D0E4F5;
}
.table-minimal thead th:first-child {
  border-left: none;
}

.table-minimal tfoot td {
  font-size: 14px;
}

// buttons
.bttn[disabled] {
 cursor: not-allowed;
 color:grey;
 border-color:grey;
 background-color: white;
}

.btn-default {
   background-color: #1d89ff !important;
   color: #fff;
   border-color: #1d89ff;
}
.btn-default:hover {
   background-color: #fff!important;
   color: #1d89ff;
   border-color: #1d89ff;
}

.btn-disabled {
   background-color: #888!important;
   color: #000;
   border-color: #888;
}

.btn-disabled:hover {
   background-color: #888!important;
   color: #000;
   border-color: #888;
}
.btn-success {
   background-color: #00a65a !important;
   color: #fff;
   border-color: #00a65a;
}

.btn-success .fa-circle-check {
   color: #fff;
}

.btn-success:hover {
   background-color: #00a65a !important;
   color: #fff;
   border-color: #00a65a;
}

.btn-danger {
   background-color: #ab0f0e !important;
   color: #fff;
   border-color: #ab0f0e;
}

.btn-danger .fa-circle-check {
   color: #fff;
}

.btn-danger:hover {
   background-color: #ab0f0e !important;
   color: #fff;
   border-color: #ab0f0e;
}
.btn-warning {
   background-color: #cf4d03 !important;
   color: #fff;
   border-color: #cf4d03;
}

.btn-warning .fa-circle-check {
   color: #fff;
}

.btn-warning:hover {
   background-color: #cf4d03 !important;
   color: #fff;
   border-color: #cf4d03;
}

.vrtc-tab-panel-menu {
width: 10%;
}

div.vrtc-tab-panel-container {
margin-top: 0px;
}

.list-group-item {
 padding: 0px 15px;
}

## .caption-box {
## margin-left: 80px;
## }
.caption-box {
border: 1px solid black;
padding: 10px;
}
#log_out {
display:flex;
flex-direction: column-reverse;
height: 200px;
line-height:1.5rem;
}

.shiny-input-radiogroup {
display:inline-flex;
padding-left: 10px;
margin-bottom:0px;
position:relative;
}

.overlay {
position: fixed;
top: 0; left: 0; width: 100%; height: 100%;
             background-color: rgba(0, 0, 0, 0.7); 
             color: white;
text-align: left;
font-size: 24px;
z-index:9999;
}

.overlay > div {
left:50%;
top:50%;
transform: translate(-50%, -50%);
position:relative;
}

.overlay #log_out {
display:flex;
flex-direction: column-reverse;
height: 500px;
line-height:1.5rem;
}

#wait_message {
display:none;
}

.overlay #wait_message {
display:inline;
right:20px;
position:absolute;
}

.refresh_button {
width:40px;
padding:5px;
margin-top:25px;
height:35px;
}
    '
)) 
