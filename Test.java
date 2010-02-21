a = 10;
print("test");
System.out.println("Hello, world");
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.border.*;
public class SlotMachine extend JFrame implements ActionListener {
public static void main(String[] args) {
JFrame frame  = new SlotMachine()frame.setBounds(0, 0, 640, 480);
frame.setVisible(true);
frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
}
private Random random ;
private javax.swing.Timer timer ;
private JPanel labelPanel,buttonPanel ;
private boolean[] isStopLabels ;
private JLabel[] labels ;
private JButton[] buttons ;
private JButton startButton ;
public SlotMachine() {
super("スロットマシーン");
labelPanel = new JPanel();
buttonPanel = new JPanel();
labelPanel.setLayout(new GridLayout(1, 3));
buttonPanel.setLayout(new GridLayout(1, 3));
}
}
