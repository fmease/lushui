use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet},
};

pub struct Node<'a>(NodeKind<'a>);

impl<'a> Node<'a> {
    pub fn verbatim(content: impl Into<Cow<'a, str>>) -> Self {
        Self(NodeKind::Verbatim(content.into()))
    }

    pub fn render(self, output: &mut String) {
        match self.0 {
            NodeKind::Element(element) => element.render(output),
            NodeKind::VoidElement(element) => element.render(output),
            NodeKind::Text(text) => {
                html_escape::encode_text_to_string(text, output);
            }
            NodeKind::Verbatim(verbatim) => *output += &verbatim,
        }
    }
}

impl<'a> From<Element<'a>> for Node<'a> {
    fn from(element: Element<'a>) -> Self {
        Self(NodeKind::Element(element))
    }
}

impl<'a> From<VoidElement<'a>> for Node<'a> {
    fn from(element: VoidElement<'a>) -> Self {
        Self(NodeKind::VoidElement(element))
    }
}

impl<'a, S: Into<Cow<'a, str>>> From<S> for Node<'a> {
    fn from(text: S) -> Self {
        Self(NodeKind::Text(text.into()))
    }
}

enum NodeKind<'a> {
    Element(Element<'a>),
    VoidElement(VoidElement<'a>),
    Text(Cow<'a, str>),
    Verbatim(Cow<'a, str>),
}

#[derive(Default)]
pub struct Document<'a> {
    children: NodeList<'a>,
}

impl<'a> Document<'a> {
    #[allow(dead_code)]
    pub fn child(mut self, child: impl Into<Node<'a>>) -> Self {
        self.add_child(child);
        self
    }

    pub fn add_child(&mut self, child: impl Into<Node<'a>>) {
        self.children.0.push(child.into());
    }

    pub fn render(self, output: &mut String) {
        *output += "<!doctype html>";
        self.children.render(output);
    }
}

pub struct Element<'a> {
    tag: Cow<'a, str>,
    attributes: Attributes<'a>,
    children: NodeList<'a>,
}

impl<'a> Element<'a> {
    pub fn new(tag: impl Into<Cow<'a, str>>) -> Self {
        Self {
            tag: tag.into(),
            attributes: Attributes::default(),
            children: NodeList::default(),
        }
    }

    pub fn child(mut self, child: impl Into<Node<'a>>) -> Self {
        self.add_child(child);
        self
    }

    pub fn add_child(&mut self, child: impl Into<Node<'a>>) {
        self.children.0.push(child.into());
    }

    pub fn render(self, output: &mut String) {
        *output += "<";
        *output += &self.tag;
        self.attributes.render(output);
        *output += ">";

        self.children.render(output);

        *output += "</";
        *output += &self.tag;
        *output += ">";
    }
}

impl<'a> Attributable<'a> for Element<'a> {
    fn attributes(&mut self) -> &mut Attributes<'a> {
        &mut self.attributes
    }
}

pub struct VoidElement<'a> {
    tag: Cow<'a, str>,
    attributes: Attributes<'a>,
}

impl<'a> VoidElement<'a> {
    pub fn new(tag: impl Into<Cow<'a, str>>) -> Self {
        Self {
            tag: tag.into(),
            attributes: Attributes::default(),
        }
    }

    pub fn render(self, output: &mut String) {
        *output += "<";
        *output += &self.tag;
        self.attributes.render(output);
        *output += ">";
    }
}

impl<'a> Attributable<'a> for VoidElement<'a> {
    fn attributes(&mut self) -> &mut Attributes<'a> {
        &mut self.attributes
    }
}

#[derive(Default)]
struct NodeList<'a>(Vec<Node<'a>>);

impl NodeList<'_> {
    fn render(self, output: &mut String) {
        for node in self.0 {
            node.render(output);
        }
    }
}

#[derive(Default)]
pub struct Attributes<'a> {
    key_value: BTreeMap<Cow<'a, str>, Cow<'a, str>>,
    boolean: BTreeSet<Cow<'a, str>>,
}

impl Attributes<'_> {
    fn render(self, output: &mut String) {
        for (name, value) in self.key_value {
            *output += " ";
            *output += &name;
            *output += "=\"";
            html_escape::encode_quoted_attribute_to_string(value, output);
            *output += "\"";
        }

        for name in self.boolean {
            *output += " ";
            *output += &name;
        }
    }
}

pub trait Attributable<'a>: Sized {
    fn attributes(&mut self) -> &mut Attributes<'a>;

    fn add_attribute(&mut self, name: &'a str, value: impl Into<Cow<'a, str>>) {
        self.attributes()
            .key_value
            .insert(name.into(), value.into());
    }

    fn add_boolean_attribute(&mut self, name: &'a str) {
        self.attributes().boolean.insert(name.into());
    }

    fn attribute(mut self, name: &'a str, value: impl Into<Cow<'a, str>>) -> Self {
        self.add_attribute(name, value);
        self
    }

    fn boolean_attribute(mut self, name: &'a str) -> Self {
        self.add_boolean_attribute(name);
        self
    }

    fn class(mut self, name: &'a str) -> Self {
        self.add_class(name);
        self
    }

    fn add_class(&mut self, name: &'a str) {
        let classes = self
            .attributes()
            .key_value
            .entry("class".into())
            .or_default()
            .to_mut();
        *classes += " ";
        *classes += name;
    }
}
