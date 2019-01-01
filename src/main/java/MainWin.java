
import automata.DFA;
import automata.NFA;
import regexParser.RegexTree;
import viewer.AutomataViewer$;
import viewer.HorizontalTreeViewer$;
import xlex.CppConverter;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import regexParser.RegexStackParser$;

public class MainWin {
    private JTextField regexTextField;
    private JButton parseButton;
    private JButton openButton;
    private JButton saveAsButton;
    private JButton DFAButton;
    private JButton NFAButton;
    private JButton minDFAButton;
    private JButton regexTreeButton;
    private JButton quitButton;
    private JButton cCodeButton;
    private JPanel panel;
    private JEditorPane editorPane;

    private JFileChooser chooser = new JFileChooser();

    private RegexTree regexTree;
    private NFA nfa;
    private DFA dfa;
    private DFA minDfa;
    private String cCode;

    private boolean regexIsValid = false;

    public static void main(String[] args) {
        try {
            UIManager.setLookAndFeel("com.sun.java.swing.plaf.gtk.GTKLookAndFeel");
        } catch (Exception e) {}

        SwingUtilities.invokeLater(() -> {
            JFrame frame = new JFrame("MainWin");
            frame.setContentPane(new MainWin().panel);
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            Dimension dimension = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
            frame.setSize(dimension.width/2, dimension.height/2);
            frame.setVisible(true);
        });
    }

    public MainWin() {
        editorPane.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 16));

        parseButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                onParseButtonClick();
            }
        });

        regexTextField.addPropertyChangeListener(new PropertyChangeListener() {
            @Override
            public void propertyChange(PropertyChangeEvent evt) {
                regexIsValid = false;
            }
        });
        cCodeButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                onCCodeButtonClick();
            }
        });
        regexTreeButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                if (!regexIsValid) return;
                editorPane.setText(HorizontalTreeViewer$.MODULE$.fromRegexTree(regexTree));
            }
        });
        NFAButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                if (!regexIsValid) return;
                editorPane.setText(AutomataViewer$.MODULE$.prettyStyle(nfa));
            }
        });
        DFAButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                if (!regexIsValid) return;
                editorPane.setText(AutomataViewer$.MODULE$.prettyStyle(dfa));
            }
        });
        minDFAButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                if (!regexIsValid) return;
                editorPane.setText(AutomataViewer$.MODULE$.prettyStyle(minDfa));
            }
        });
        openButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);

            }
        });
        openButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                int returnVal = chooser.showOpenDialog(panel);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    String path = chooser.getSelectedFile().getAbsolutePath();
                    try {
                        java.util.List<String> strs = Files.readAllLines(Paths.get(path));
                        StringBuilder result = new StringBuilder();
                        for (String s : strs) {
                            result.append(s);
                            result.append("\n");
                        }
                        editorPane.setText(result.toString());
                    } catch (IOException exception) {
                        exception.printStackTrace();
                    }
                }
            }
        });
        saveAsButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                int resultVal = chooser.showSaveDialog(panel);
                if (resultVal == JFileChooser.APPROVE_OPTION) {
                    File output = chooser.getSelectedFile();
                    try (Writer writer = new BufferedWriter(new FileWriter(output))) {
                        writer.write(editorPane.getText());
                    } catch (IOException e1) {

                    }
                }
            }
        });
        quitButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                super.mouseClicked(e);
                System.exit(0);
            }
        });
    }

    private void onParseButtonClick() {
        try {
            if (regexTextField.getText().isEmpty()) return;
            regexTree = RegexStackParser$.MODULE$.buildRegexTree(regexTextField.getText());
            nfa = NFA.fromRegexTree(regexTree);
            dfa = DFA.fromNFA(nfa);
            minDfa = DFA.minimize(dfa);
            cCode = CppConverter.convert(dfa,"recognize", "getSymbol", "retract");
            regexIsValid = true;
            onCCodeButtonClick();
        } catch (Exception e) {
            e.printStackTrace();
            editorPane.setText("Parse error!");
        }
    }

    private void onCCodeButtonClick() {
        if (!regexIsValid) return;
        editorPane.setText(cCode);
    }

}
